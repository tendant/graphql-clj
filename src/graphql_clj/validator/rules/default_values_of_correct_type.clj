(ns graphql-clj.validator.rules.default-values-of-correct-type
  "A GraphQL document is only valid if all variable default values are of the type expected by their definition."
  (:require [graphql-clj.validator.errors :as ve]
            [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.box :as box]))

(defn- default-for-required-arg-error [{:keys [variable-name default-value type-name]}]
  {:error (format "Variable '$%s' of type '%s!' is required and will never use the default value. Perhaps you meant to use type '%s'."
                  (box/box->val variable-name) type-name type-name)
   :loc   (ve/extract-loc (meta default-value))})

(defnodevisitor default-for-required-field :pre :variable-definition
  [{:keys [required default-value] :as n} s]
  (when (and required default-value)
    {:state (ve/update-errors s (default-for-required-arg-error n))}))

(defn- bad-value-for-default-error [{:keys [variable-name default-value type-name spec]}]
  {:error (format "Variable '$%s' of type '%s' has invalid default value: %s. Reason: %s."
                  (box/box->val variable-name) type-name (ve/render (box/box->val default-value)) (ve/explain-invalid spec (box/box->val default-value)))
   :loc   (ve/extract-loc (meta default-value))})

(defnodevisitor bad-value-for-default :pre :variable-definition
  [{:keys [spec default-value v/path] :as n} s]
  (when (and spec default-value (not (ve/valid? spec (box/box->val default-value) path)))
    {:state (ve/update-errors s (bad-value-for-default-error n))}))

(def rules
  [default-for-required-field
   bad-value-for-default])
