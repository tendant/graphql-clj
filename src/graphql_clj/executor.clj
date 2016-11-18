(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [graphql-clj.validator.errors :as ve]
            [clojure.set :as set]
            [graphql-clj.validator :as validator]
            [clojure.string :as str]))

(defn- resolve-field-on-object
  [{:keys [parent-type-name field-name v/args-fn]} {:keys [context resolver variables]} parent-object]
  (let [resolver (resolver parent-type-name field-name)]    ;; TODO will be a breaking change, but prepare the resolver fns at the end of the validation phase
    (resolver context parent-object (when args-fn (args-fn variables)))))

(declare execute-fields)

(defn- complete-value
  [{:keys [selection-set kind of-kind] :as field-entry} state result]
  (when result ;;TODO not null type is borked
    (cond
      (#{:SCALAR :ENUM} kind)             result
      (#{:OBJECT :INTERFACE :UNION} kind) (execute-fields selection-set state result)
      (#{:LIST} kind)                     (map #(complete-value (merge field-entry of-kind) state %) result))))

(defn- get-field-entry [{:keys [name field-name] :as field-entry} state parent-object]
  [(or name field-name) (->> (resolve-field-on-object field-entry state parent-object)
                             (complete-value field-entry state))])

(defn- execute-fields
  [fields state root-value]
  (->> fields (map #(get-field-entry % state root-value)) (into {})))

(defn- guard-missing-vars! [{:keys [variable-definitions]} {:keys [variables]}]
  (let [required-variables (->> (remove :default-value variable-definitions) (map :variable-name) set)
        input-variables    (set (map name (keys variables)))
        missing-variables  (set/difference required-variables input-variables)]
    (when-not (empty? missing-variables)
      (gerror/throw-error (format "Missing input variables (%s)." (str/join "," missing-variables))))))

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

(defn- prepare [schema-or-state resolver-fn statement-or-state]
  (let [state (schema-or-state->state schema-or-state)]
    (ve/guard-errors! state)
    (let [validated-statement (statement-or-state->state state statement-or-state)]
      (ve/guard-errors! (:state validated-statement))
      (let [resolver (resolver/create-resolver-fn (:schema (:state validated-statement)) resolver-fn)]
        (assoc-in validated-statement [:state :resolver] resolver)))))

;; Public API

(defn execute
  ([context schema-or-state resolver-fn statement-or-state]
   (execute context schema-or-state resolver-fn statement-or-state nil))
  ([context schema-or-state resolver-fn statement-or-state variables]
   (try (-> (prepare schema-or-state resolver-fn statement-or-state)
            (assoc-in [:state :context] context)
            (assoc-in [:state :variables] variables)
            execute-document)
        (catch Exception e
          (if-let [error (ex-data e)]
            (if (map? error) error {:errors [error]})
            (throw e))))))
