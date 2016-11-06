(ns graphql-clj.validator.rules.lone-anonymous-operation
  "A GraphQL document is only valid if when it contains an anonymous operation
  (the query short-hand) that it contains only that one operation definition."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn multiple-anonymous-ops-error [ops]
  {:error "This anonymous operation must be the only defined operation."
   :loc (ve/extract-loc (meta (first ops)))})

(defnodevisitor multiple-anonymous-ops :pre :query-root
  [{:keys [children] :as n} s]
  (let [anonymous-ops (filter #(and (:operation-type %) (-> % :operation-type :name nil?)) children)]
    (when (> (count anonymous-ops) 1)
      {:state (ve/update-errors s (multiple-anonymous-ops-error anonymous-ops))
       :break true})))

(def rules [multiple-anonymous-ops])
