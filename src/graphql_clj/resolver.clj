(ns graphql-clj.resolver
  (:require [clojure.core.match :as match]
            [graphql-clj.type :as type]))

(defn default-resolver-fn [type-name field-name]
  (fn [context parent & args]
    (assert type-name (format "type name is NULL for field: %s." field-name))
    (assert field-name (format "field-name is NULL for tyep: %s." type-name))
    (get parent (keyword field-name))))

(defn schema-introspection-resolver-fn
  [schema]
  (let [root-query-type (type/get-root-query-type schema)
        root-query-name (:name root-query-type)]
    (fn [type-name field-name]
      (match/match
       [type-name field-name]
       [root-query-name "__schema"] (fn [context parent & args]
                                      {:types nil ;(vals (:types schema))
                                       :queryType root-query-type
                                       :mutationType nil
                                       :directives nil})
       [root-query-name "__type"] (fn [context parent & args]
                                    {:kind nil
                                     :name nil
                                     :description nil
                                     :fields nil
                                     :interfaces nil
                                     :possibleTypes nil
                                     :enumValues nil
                                     :inputFields nil
                                     :ofType nil})
       ["__Schema" "types"] (fn [context parent & rest]
                              [])
       :else nil))))

(defn create-resolver-fn
  [schema resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn schema)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
