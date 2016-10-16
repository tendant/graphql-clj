(ns graphql-clj.validator.rules.unique-input-field-names
  "A GraphQL input object value is only valid if all supplied fields are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]))

(defn- duplicate-input-field-name-error [input-field-name]
  (format "There can be only one input field named '%s'." input-field-name))

(defnodevisitor duplicate-input-field-name-schema :pre :input-definition ;; TODO test
  [{:keys [fields] :as n} s]
  (let [duplicate-name-errors (->> (map :field-name fields)
                                   ve/duplicates
                                   (map duplicate-input-field-name-error))]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(defn vec->map [k-key v-key [_ v]] (->> (map #(vector (k-key %) (v-key %)) v) (into {})))
(defn object-value? [v] (and (vector? v) (= :object-value (first v))))

(defnodevisitor duplicate-input-field-name-op :pre :argument
  [{:keys [value] :as n} s]
  (when (object-value? value)
    (let [duplicate-name-errors (->> (last value)
                                     (map :name)
                                     ve/duplicates
                                     (map duplicate-input-field-name-error))]
      (if (empty? duplicate-name-errors)
        {:node (update n :value (partial vec->map :name :value))}
        {:state (apply ve/update-errors s duplicate-name-errors)
         :break true}))))

(def rules [duplicate-input-field-name-schema
            duplicate-input-field-name-op])
