(ns graphql-clj.validator.rules.unique-fragment-names
  "A GraphQL document is only valid if all defined fragments have unique names."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-fragment-name-error [fragment-name]
  (format "There can be only one fragment named '%s'." fragment-name))

(defnodevisitor duplicate-fragment-name :pre :query-root
  [{:keys [children] :as n} s]
  (let [duplicate-name-errors (->> (filter #(= :fragment-definition (:node-type %)) children)
                                   (map :name)
                                   ve/duplicates
                                   (map duplicate-fragment-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(def rules [duplicate-fragment-name])
