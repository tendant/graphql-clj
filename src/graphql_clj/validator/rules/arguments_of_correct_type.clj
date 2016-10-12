(ns graphql-clj.validator.rules.arguments-of-correct-type
  "A GraphQL document is only valid if all field argument literal values are of the type expected by their position."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.spec :as spec]
            [graphql-clj.validator.errors :as ve]
            [clojure.spec :as s]))

(defn- bad-value-error [{:keys [spec argument-name value]}]
  (let [type-name (spec/get-spec-name (s/get-spec spec))]
    (format "Argument '%s' of type '%s' has invalid value: %s. Reason: %s."
            argument-name type-name (ve/render value) (ve/explain-invalid spec value))))

(defnodevisitor bad-value :post :argument
  [{:keys [spec value] :as n} s]
  (when (and spec value (not (s/valid? spec value)))
    {:state (ve/update-errors s (bad-value-error (assoc n :value value)))}))

(def rules [bad-value])
