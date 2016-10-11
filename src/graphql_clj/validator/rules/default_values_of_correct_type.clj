(ns graphql-clj.validator.rules.default-values-of-correct-type
  "A GraphQL document is only valid if all variable default values are of the type expected by their definition."
  (:require [clojure.spec :as s]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.visitor :refer [defnodevisitor]]))

(defn- default-for-required-arg-error [{:keys [variable-name type-name]}]
  (format "Variable '$%s' of type '%s!' is required and will never use the default value. Perhaps you meant to use type '%s'."
          variable-name type-name type-name))

(defnodevisitor default-for-required-field :post :variable-definition
  [{:keys [required default-value] :as n} s]
  (when (and required default-value)
    {:state (ve/update-errors s (default-for-required-arg-error n))}))

(defn- bad-value-for-default-error [{:keys [variable-name default-value type-name]}]
  (format "Variable '$%s' of type '%s' has invalid default value: %s. Reason: %s."
          variable-name type-name (ve/render default-value) (ve/render-type-expectation type-name)))

(defnodevisitor bad-value-for-default :post :variable-definition
  [{:keys [spec default-value] :as n} s]
  (when (and spec default-value (not (s/valid? spec default-value)))
    {:state (ve/update-errors s (bad-value-for-default-error n))}))

(def rules
  [default-for-required-field
   bad-value-for-default])
