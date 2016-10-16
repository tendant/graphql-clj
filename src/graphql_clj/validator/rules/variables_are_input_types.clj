(ns graphql-clj.validator.rules.variables-are-input-types
  "A GraphQL operation is only valid if all the variables it defines are of input types (scalar, enum, or input object)."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn- bad-variable-type-error [{:keys [variable-name]} {:keys [spec]}]
  (format "Variable '$%s' cannot be non-input type '%s'." variable-name (name spec)))

(def acceptable-types #{:scalar :enum-definition :input-definition})

(defnodevisitor bad-variable-type :pre :variable-definition [n s]
  (let [{:keys [node-type] :as type-node} (spec/get-base-type-node n s)]
    (when-not (acceptable-types node-type)
      {:state (ve/update-errors s (bad-variable-type-error n type-node))})))

(def rules [bad-variable-type])
