(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [graphql-clj.validator.errors :as ve]
            [clojure.set :as set]
            [graphql-clj.validator :as validator]
            [clojure.string :as str]))

(defn- resolve-field-on-object
  [{:keys [resolver-fn parent-type-name field-name args-fn]} {:keys [context resolver vars]} parent-result]
  (let [resolve (or resolver-fn (resolver parent-type-name field-name))]
    (resolve context parent-result (when args-fn (args-fn vars)))))

(declare execute-fields)

(defn- complete-value
  [{:keys [selection-set field-name kind of-kind required] :as field-entry} state result]
  (when (and required (nil? result))
    (gerror/throw-error (format "NOT_NULL field \"%s\" assigned a null value." field-name)))
  (when result
    (cond
      (#{:SCALAR :ENUM} kind)             result
      (#{:OBJECT :INTERFACE :UNION} kind) (execute-fields selection-set state result)
      (#{:LIST} kind)                     (map #(complete-value (merge field-entry of-kind) state %) result))))

(defn- get-field-entry [{:keys [name field-name] :as field-entry} state parent-result]
  [(or name field-name) (->> (resolve-field-on-object field-entry state parent-result)
                             (complete-value field-entry state))])

(defn- execute-fields
  [fields state root-value]
  (->> fields (map #(get-field-entry % state root-value)) (into {})))

(defn- guard-missing-vars! [{:keys [variable-definitions]} {:keys [vars]}]
  (let [required-vars (->> (remove :default-value variable-definitions) (map :variable-name) set)
        input-vars    (set (map name (keys vars)))
        missing-vars  (set/difference required-vars input-vars)]
    (when-not (empty? missing-vars)
      (gerror/throw-error (format "Missing input variables (%s)." (str/join "," missing-vars))))))

(defn- execute-statement [{:keys [selection-set] :as document} state]
  (guard-missing-vars! document state)
  (execute-fields selection-set state :root))

(defn- execute-document
  [{:keys [document state]}]
  (let [operation-definitions (:operation-definitions document)]
    {:data (into {} (map #(execute-statement % state) operation-definitions))}))

(defn- schema-or-state->state
  "Schema validation inside execution phase for backwards compatibility"
  [schema-or-state]
  (if (or (:errors schema-or-state) (:spec-map schema-or-state))
    schema-or-state
    (do (prn "Warning: the result of graphql-clj.validator/validate-schema should be passed to execute instead of a schema string")
        (validator/validate-schema schema-or-state))))

(defn- statement-or-state->state
  "Statement parsing and validation inside execution phase for backwards compatibility"
  [state statement-or-state]
  (if (string? statement-or-state)
    (do (prn "Warning: the result of graphql-clj.validator/validate-statement should be passed to execute instead of a statement string")
        (-> statement-or-state parser/parse (validator/validate-statement state)))
    statement-or-state))

(defn- prepare [schema-or-state statement-or-state]
  (let [state (schema-or-state->state schema-or-state)]
    (ve/guard-errors! state)
    (let [validated-statement (statement-or-state->state state statement-or-state)]
      (ve/guard-errors! validated-statement)
      validated-statement)))

;; Public API

(defn execute
  ([context schema-or-state resolver-fn statement-or-state]
   (execute context schema-or-state resolver-fn statement-or-state nil))
  ([context schema-or-state resolver-fn statement-or-state variables]
   (try
     (let [validated-statement (prepare schema-or-state statement-or-state)
           state (assoc schema-or-state :context context
                                        :vars variables
                                        :resolver (resolver/create-resolver-fn schema-or-state resolver-fn))]
       (execute-document (assoc validated-statement :state state)))
     (catch Exception e
       (if-let [error (ex-data e)]
         (if (map? error) error {:errors [error]})
         (throw e))))))
