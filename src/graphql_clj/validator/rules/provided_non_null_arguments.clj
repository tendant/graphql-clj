(ns graphql-clj.validator.rules.provided-non-null-arguments
  "A field or directive is only valid if all required (non-null) field arguments have been provided."
  (:require [graphql-clj.visitor :refer [defnodevisitor]]
            [graphql-clj.validator.errors :as ve]
            [graphql-clj.spec :as spec]))

(defn missing-argument-error [type-node n {:keys [argument-name type-name]}]
  {:error (format "Field '%s' argument '%s' of type '%s' is required but not provided."
                  (:field-name type-node) argument-name type-name)
   :loc (ve/extract-loc (meta n))})

(defnodevisitor non-null-arguments :post :field
  [{:keys [v/parent required spec arguments] :as n} s]
  (let [type-node (spec/get-type-node spec s)
        required-args (filter :required (:arguments type-node))]
    (when-let [values (and (not (empty? required-args))
                           (->> arguments
                                (map #(vector (:argument-name %) (:value %)))
                                (into {})))]
      (let [errors (some->> required-args
                            (map #(when-not (get values (:argument-name %)) %))
                            (remove nil?)
                            (map (partial missing-argument-error type-node n)))]
        (when-not (empty? errors)
          {:state (apply ve/update-errors s errors)})))))

(def rules [non-null-arguments])
