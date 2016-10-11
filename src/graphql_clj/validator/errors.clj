(ns graphql-clj.validator.errors
  (:require [graphql-clj.type :as t]))

(defn- conj-error [error errors]
  (conj (or errors []) {:error error}))

(defn update-errors [ast error]
  (update ast :errors (partial conj-error error))) ;; TODO add :loc once parse spans are preserved

(defn render [v] (if (string? v) (str "\"" v "\"") v))

(defn render-type-expectation [type-name]
  (if (t/default-type-names type-name)
    (str type-name " value expected")                               ;; TODO enum, etc?
    (str (format "Expected '%s', found not an object" type-name)))) ;; TODO not an object is only one possible failure mode for object types
