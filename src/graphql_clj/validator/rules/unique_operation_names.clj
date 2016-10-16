(ns graphql-clj.validator.rules.unique-operation-names
  "A GraphQL document is only valid if all defined operations have unique names."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-operation-name-error [operation-name]
  (format "There can be only one operation named '%s'." operation-name))

(defnodevisitor duplicate-operation-name :pre :query-root
  [{:keys [children] :as n} s]
  (let [duplicate-name-errors (->> (filter #(= :operation-definition (:node-type %)) children)
                                   (map #(get-in % [:operation-type :name] "Query"))
                                   ve/duplicates
                                   (map duplicate-operation-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(def rules [duplicate-operation-name])
