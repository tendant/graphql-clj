(ns graphql-clj.validator.rules.lone-anonymous-operation
  "A GraphQL document is only valid if when it contains an anonymous operation
  (the query short-hand) that it contains only that one operation definition."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(def multiple-anonymous-ops-error
  {:error "This anonymous operation must be the only defined operation."})

(defnodevisitor multiple-anonymous-ops :pre :query-root
  [{:keys [children]} s]
  (when (> (->> (filter :operation-type children)
              (map (comp :name :operation-type))
              (filter nil?)
              count) 1)
    {:state (ve/update-errors s multiple-anonymous-ops-error)
     :break true}))

(def rules [multiple-anonymous-ops])
