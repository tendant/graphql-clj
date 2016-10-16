(ns graphql-clj.validator.rules.unique-input-field-names
  "A GraphQL input object value is only valid if all supplied fields are uniquely named."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [clojure.walk :as walk]))

(defn- duplicate-input-field-name-error [input-field-name]
  (format "There can be only one input field named '%s'." input-field-name))

(defn- duplicate-names [k s]
  (->> (map k s) ve/duplicates (map duplicate-input-field-name-error)))

(defnodevisitor duplicate-input-field-name-schema :pre :input-definition
  [{:keys [fields] :as n} s]
  (let [duplicate-name-errors (duplicate-names :field-name fields)]
    (when-not (empty? duplicate-name-errors)
      {:state (apply ve/update-errors s duplicate-name-errors)
       :break true})))

(defn- object-value? [v]
  (and (vector? v) (= :object-value (first v))))

(defn- object-value->map [v]
  (cond (map? v)                              [(keyword (:name v)) (:value v)]
        (and (vector? v) (vector? (first v))) (into {} v)
        :else                                 v))

(defn- duplicate-input-operation
  "Check an object value for duplicate keys.  If there are none, convert the object value to a map."
  [k n s]
  (when (object-value? (k n))
    (let [duplicate-name-errors (duplicate-names :name (last (k n)))]
      (if (empty? duplicate-name-errors)
        {:node (update n k #(walk/postwalk object-value->map (last %)))}
        {:state (apply ve/update-errors s duplicate-name-errors)
         :break true}))))

(defnodevisitor duplicate-input-field-name-var :pre :variable-definition
  [{:keys [default-value] :as n} s]
  (duplicate-input-operation :default-value n s))

(defnodevisitor duplicate-input-field-name-arg :pre :argument
  [{:keys [value] :as n} s]
  (duplicate-input-operation :value n s))

(def rules [duplicate-input-field-name-schema
            duplicate-input-field-name-arg
            duplicate-input-field-name-var])
