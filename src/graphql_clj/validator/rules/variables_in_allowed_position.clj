(ns graphql-clj.validator.rules.variables-in-allowed-position
  "Variables passed to field arguments conform to type"
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]
            [graphql-clj.spec :as spec]))

(defn- variable-type-error [variable-name var-type arg-type]
  (format "Variable '$%s' of type '%s' used in position expecting type '%s'."
          variable-name (name var-type) (name arg-type)))

(defn subtype-of
  "A var type is allowed if it is the same or more strict (e.g. is
   a subtype of) than the expected type. It can be more strict if
   the variable type is non-null when the expected type is nullable.
   If both are list types, the variable item type can be more strict
   than the expected item type (contravariant)."
  [_ maybe-sub-type super-type]                             ;; TODO sloppy, incomplete
  (or (= (spec/remove-required (namespace maybe-sub-type) (name maybe-sub-type)) super-type)
      (= maybe-sub-type super-type)))

(defn- effective-type
  "If a variable definition has a default value, it's effectively non-null."
  [variable-type {:keys [default-value]}]
  (if (and default-value (not (s/valid? #nu/tapd variable-type nil)))
    (spec/add-required (namespace variable-type) (name variable-type))
    variable-type))

(defnodevisitor variable-type-mismatch :post :argument
  [{:keys [spec variable-name] :as n} s]
  (let [arg-type (s/get-spec spec)
        var-spec (spec/spec-for s {:node-type :variable-usage :variable-name variable-name})
        var-type (s/get-spec var-spec)
        var-def  (spec/get-type-node var-spec s)]
    (when-not (subtype-of (:schema s) (effective-type var-type var-def) arg-type)
      {:state (ve/update-errors s (variable-type-error variable-name var-type arg-type))})))

(def rules [variable-type-mismatch])
