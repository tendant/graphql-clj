(ns graphql-clj.validator.rules.arguments-of-correct-type
  "A GraphQL document is only valid if all field argument literal values are of the type expected by their position."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.box :as box]
            [clojure.spec :as s]))

(defn- bad-value-error [{:keys [spec argument-name value]}]
  (let [type-name (name (s/get-spec spec))]
    (format "Argument '%s' of type '%s' has invalid value: %s. Reason: %s."
            argument-name type-name (ve/unboxed-render value) (ve/explain-invalid spec value))))

(defnodevisitor bad-value :pre :argument
  [{:keys [spec value v/path] :as n} s]
  (if (and spec value (not (ve/valid? spec (box/box->val value) path)))
    {:state (ve/update-errors s (bad-value-error n))
     :node (update n :value box/box->val)}
    {:node (update n :value box/box->val)}))

(def rules [bad-value])
