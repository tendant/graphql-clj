(ns graphql-clj.validator.rules.unique-fragment-names
  "A GraphQL document is only valid if all defined fragments have unique names."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defnodevisitor duplicate-fragment-name :pre :statement-root
  [{:keys [children] :as n} s]
  (ve/guard-duplicate-names "fragment" :name (filter #(= :fragment-definition (:node-type %)) children) s))

(def rules [duplicate-fragment-name])
