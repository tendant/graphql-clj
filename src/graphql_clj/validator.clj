(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]))

(def specified-rules
  (->> [graphql-clj.validator.rules.default-values-of-correct-type/validate]
       (reduce comp identity)))

(defn validate [schema ast]
  ast)
