(ns graphql-clj.validator.rules.variables-are-input-types
  "A GraphQL operation is only valid if all the variables it defines are of input types (scalar, enum, or input object)."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]))

(defn- bad-variable-type-error [{:keys [spec argument-name value]}]
  (let [type-name (name (s/get-spec spec))]
    (format "Argument '%s' of type '%s' has invalid value: %s. Reason: %s."
            argument-name type-name (ve/render value) (ve/explain-invalid spec value))))

(defnodevisitor bad-variable-type :pre :variable-definition
  [{:keys [spec value v/path] :as n} s]
  (when (and spec value (not (ve/valid? spec value path)))
    {:state (ve/update-errors s (bad-variable-type-error (assoc n :value value)))}))

(def rules [bad-variable-type])
