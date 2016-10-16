(ns graphql-clj.validator.rules.unique-variable-names
  "A GraphQL operation is only valid if all its variables are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-variable-name-error [variable-name]
  (format "There can be only one variable named '$%s'." variable-name))

(defnodevisitor duplicate-variable-name :pre :operation-definition
  [{:keys [variable-definitions] :as n} s]
  (let [duplicate-name-errors (->> (map :variable-name variable-definitions)
                                   ve/duplicates
                                   (map duplicate-variable-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(def rules [duplicate-variable-name])
