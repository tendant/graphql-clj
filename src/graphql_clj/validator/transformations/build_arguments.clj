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

(defn- var-args->defaults [var-args s]
  (->> (map #(vector % (some-> (spec/spec-for-var-usage % s)
                               (spec/get-type-node s)
                               :default-value
                               box/box->val)) (vals var-args)) (into {})))

(defn- var-args->args
  "Bind arguments with variable values (at execution time)"
  [args var-args var-arg-defaults vars]
  (->> var-args (map (fn [[k v]] [k (get vars v (get var-arg-defaults v))])) (into {}) (merge args)))

(defn- args->args-fn [type-field-args args s]
  "Return a function of variables that can be run at execution time to produce an args key value map"
  (let [var-args    (mapify-args :variable-name args)
        merged-args (merge (mapify-args :default-value type-field-args) (mapify-args :value args))]
    (if (empty? var-args)
      (constantly merged-args)
      (partial var-args->args merged-args var-args (var-args->defaults var-args s)))))

(declare field-arguments)
(v/defnodevisitor field-arguments :post :field
  [{:keys [arguments spec v/parent] :as n} {:keys [var-defs] :as s}]
  (let [type-field (spec/get-type-node spec s)]
    (when (:arguments type-field)
      {:node (assoc n :args-fn (args->args-fn (:arguments type-field) arguments s))})))

(def rules [field-arguments])
