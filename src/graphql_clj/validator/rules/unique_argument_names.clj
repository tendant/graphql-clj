(ns graphql-clj.validator.rules.unique-argument-names
  "A GraphQL field or directive is only valid if all supplied arguments are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-argument-name-error [argument-name]
  (format "There can be only one argument named '%s'." argument-name))

(defn- duplicate-argument-name [{:keys [arguments]} s]
  (let [duplicate-name-errors (->> (map :argument-name arguments)
                                   ve/duplicates
                                   (map duplicate-argument-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(defnodevisitor duplicate-argument-name-type-field :pre :type-field [n s]
  (duplicate-argument-name n s))

(defnodevisitor duplicate-argument-name-field :pre :field [n s]
  (duplicate-argument-name n s))

(defnodevisitor duplicate-argument-name-directive :pre :directive [n s]
  (duplicate-argument-name n s))

(def rules [duplicate-argument-name-type-field
            duplicate-argument-name-field
            duplicate-argument-name-directive])


