(ns graphql-clj.validator.rules.provided-non-null-arguments
  "A field or directive is only valid if all required (non-null) field arguments have been provided."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn missing-argument-error [type-node n {:keys [argument-name type-name]}]
  {:error (format "Field '%s' argument '%s' of type '%s' is required but not provided."
                  (:field-name type-node) argument-name type-name)
   :loc (ve/extract-loc (meta n))})

(defn- arg-vals
  "Create a map of {argument-name argument-value}.
   Source values from provided arguments and non-null type variables."
  [arguments s]
  (let [provided (->> (map (juxt :argument-name :value) arguments) (into {}))]
    (->> (map (juxt :argument-name :variable-name) arguments)
         (filter #(some-> % last (spec/spec-for-var-usage s) (spec/get-type-node s) :required))
         (into provided))))

(defnodevisitor non-null-arguments :post :field
  [{:keys [v/parent required spec arguments] :as n} s]
  (let [type-node     (spec/get-type-node spec s)
        required-args (filter :required (:arguments type-node))]
    (when-let [arg-vals (and (not (empty? required-args)) (arg-vals arguments s))]
      (let [errors (remove #(get arg-vals (:argument-name %)) required-args)]
        (when-not (empty? errors)
          {:state (apply ve/update-errors s (map (partial missing-argument-error type-node n) errors))})))))

(def rules [non-null-arguments])
