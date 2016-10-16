(ns graphql-clj.validator.rules.unique-operation-names
  "A GraphQL document is only valid if all defined operations have unique names."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defnodevisitor duplicate-operation-name :pre :query-root
  [{:keys [children] :as n} s]
  (ve/guard-duplicate-names "operation"
                            #(get-in % [:operation-type :name] "Query")
                            (filter #(= :operation-definition (:node-type %)) children)
                            s))

(def rules [duplicate-operation-name])
