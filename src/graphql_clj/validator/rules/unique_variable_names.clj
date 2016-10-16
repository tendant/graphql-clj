(ns graphql-clj.validator.rules.unique-variable-names
  "A GraphQL operation is only valid if all its variables are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defnodevisitor duplicate-variable-name :pre :operation-definition
  [{:keys [variable-definitions] :as n} s]
  (ve/guard-duplicate-names "variable" :variable-name variable-definitions s))

(def rules [duplicate-variable-name])
