(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [clojure.set :as set]
            [graphql-clj.validator :as validator]))

(defn- resolve-field-on-object
  [{:keys [field-name v/args-fn]} {:keys [context resolver variables]} parent-type parent-object]
  (let [parent-type-name (:type-name parent-type)
        resolver (resolver parent-type-name field-name)]    ;; TODO will be a breaking change, but prepare the resolver fns at the end of the validation phase
    (resolver context parent-object (when args-fn (args-fn variables)))))

(declare execute-fields)

(defn- complete-value
  [{:keys [selection-set] :as field-entry} {:keys [schema] :as state} {:keys [kind] :as field-type} result]
  (case kind
    :SCALAR result
    :ENUM result
    :OBJECT (execute-fields selection-set state field-type result)
    :INTERFACE (execute-fields selection-set state field-type result)
    :UNION (execute-fields selection-set state field-type result)
    :LIST (map #(complete-value field-entry state (type/get-inner-type schema field-type) %) result) ;; TODO inline
    :NOT_NULL (let [not-null-result (complete-value field-entry state (type/get-inner-type schema field-type) result)] ;; TODO inline
                (if not-null-result                         ;; TODO handle non-null as an overlay using required true
                  not-null-result
                  (gerror/throw-error (format "NOT_NULL type %s returns null." field-type)))))) ;;TODO not null type is borked

(defn- get-field-entry [{:keys [name field-name] :as field-entry} {:keys [schema] :as state} parent-type parent-object]
  (let [response-key (or name field-name)
        parent-type-name (:type-name parent-type)
        field-type (type/get-field-type schema parent-type-name field-name)] ;; TODO do not do this lookup, embed as necessary
    (let [resolved-object (resolve-field-on-object field-entry state parent-type parent-object)]
      (if (nil? resolved-object) ; when field is not-null field, resolved-object might be nil.
        [response-key nil]       ; If resolvedObject is null, return tuple(responseKey, null), indicating that an entry exists in the result map whose value is null.
        [response-key (complete-value field-entry state field-type resolved-object)]))))

(defn- execute-fields
  [fields state parent-type root-value]
  (->> fields (map #(get-field-entry % state parent-type root-value)) (into {})))

(defn- guard-missing-vars! [document {:keys [variables]}]
  (let [operation-variable-keys (set (map :variable-name (:variable-definitions document)))
        input-variable-keys     (set (map name (keys variables)))
        missing-variables       (set/difference operation-variable-keys input-variable-keys)]
    (when-not (empty? missing-variables)
      (gerror/throw-error (format "Missing variable(%s) in input variables." missing-variables)))))

(defn- execute-statement [{:keys [selection-set operation-type] :as document} {:keys [schema] :as state}]
  (guard-missing-vars! document state)
  (let [root (if (= "query" (:type operation-type)) (type/get-root-query-type schema)
                                                    (type/get-root-mutation-type schema))]
    (execute-fields selection-set state root :root)))

(defn- execute-document
  [{:keys [document state]}]
  (let [operation-definitions (:operation-definitions document)]
    {:data (into {} (map #(execute-statement % state) operation-definitions))}))

(defn- prepare [schema-or-state resolver-fn statement-or-state]
  (let [state (if (or (:errors schema-or-state) (:spec-map schema-or-state))
                schema-or-state
                (validator/validate-schema schema-or-state))] ;; Schema validation inside execution phase for backwards compatibility
    (if (:errors state)
      (select-keys state [:errors])
      (let [validated-statement (if (string? statement-or-state)
                                  (-> statement-or-state parser/parse (validator/validate-statement state))
                                  statement-or-state)]
        (if (-> validated-statement :state :errors)
          (select-keys (:state validated-statement) [:errors])
          (let [resolver (resolver/create-resolver-fn (:schema (:state validated-statement)) resolver-fn)]
            (assoc-in validated-statement [:state :resolver] resolver)))))))

;; Public API

(defn execute
  ([context schema-or-state resolver-fn statement-or-state]
   (execute context schema-or-state resolver-fn statement-or-state nil))
  ([context schema-or-state resolver-fn statement-or-state variables]
   (let [{:keys [errors] :as result} (prepare schema-or-state resolver-fn statement-or-state)]
     (if errors
       (select-keys result [:errors])
       (try
         (-> result
             (assoc-in [:state :context] context)
             (assoc-in [:state :variables] variables)
             execute-document)
         (catch Exception e
           (if-let [error (ex-data e)]
             {:errors [error]}
             (throw e))))))))
