(ns graphql-clj.validator.transformations.build-arguments
  "Eagerly build an arguments function for a field (potentially an execution time function of variables)."
  (:require [graphql-clj.visitor :as v]
            [graphql-clj.spec :as spec]
            [graphql-clj.box :as box]))

(defn- mapify-args
  "Given a vector of arguments, transform into a key value map with scrubbed values"
  [k arguments]
  (->> arguments
       (map #(when-let [result (k %)] [(box/box->val (:argument-name %)) result]))
       (remove nil?)
       (into {})))

(defn- var-args->args
  "Bind arguments with variable values (at execution time)"
  [args var-args vars]
  (->> var-args (map (fn [[k v]] [k (get vars v)])) (into {}) (merge args)))

(defn- args->args-fn [type-field-args args]
  "Return a function of variables that can be run at execution time to produce an args key value map"
  (let [var-args (mapify-args (comp keyword :variable-name) args)
        merged-args (merge (mapify-args :default-value type-field-args) (mapify-args :value args))]
    (if (empty? var-args)
      (constantly merged-args)
      (partial var-args->args merged-args var-args))))

(declare field-arguments)
(v/defnodevisitor field-arguments :post :field
  [{:keys [arguments spec v/parent] :as n} s]
  (let [type-field (spec/get-type-node spec s)]
    (when (:arguments type-field)
      {:node (assoc n :v/args-fn (args->args-fn (:arguments type-field) arguments))})))

(def rules [field-arguments])
