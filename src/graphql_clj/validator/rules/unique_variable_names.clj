(ns graphql-clj.validator.rules.unique-variable-names
  "A GraphQL operation is only valid if all variables encountered, both directly
   and via fragment spreads, are defined by that operation."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-variable-name-error [variable-name]
  (format "There can be only one variable named '$%s'." variable-name))

(defnodevisitor duplicate-variable-name :pre :operation-definition
  [{:keys [variable-definitions] :as n} s]
  (let [duplicate-name-errors (->> (map :variable-name variable-definitions)
                                   frequencies
                                   (filter (fn [[_ v]] (> v 1)))
                                   keys
                                   set
                                   (map duplicate-variable-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(def rules [duplicate-variable-name])
