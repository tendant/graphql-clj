(ns graphql-clj.resolver
  (:require [clojure.core.match :as match]
            [graphql-clj.type :as type]
            [graphql-clj.introspection :as introspection]))

(defn default-resolver-fn [type-name field-name]
  (fn [context parent & args]
    (assert type-name (format "type name is NULL for field: %s." field-name))
    (assert field-name (format "field-name is NULL for type: %s." type-name))
    (get parent (keyword field-name))))

(defn- get-root-query-type-name [schema]
  (-> schema type/get-root-query-type :type-name))

(defn schema-introspection-resolver-fn
  [schema]
  (let [root-query-name (get-root-query-type-name schema)]
    (fn [type-name field-name]
      (match/match
       [type-name field-name]
       [root-query-name "__schema"] (fn [context parent & args]
                                      {:types (introspection/schema-types schema)
                                       :queryType (introspection/type-resolver (type/get-type-in-schema schema root-query-name))
                                       :mutationType nil
                                       :directives []})
       [root-query-name "__type"] (fn [context parent & args]
                                    {:kind nil
                                     :type-name nil
                                     :description nil
                                     :fields []
                                     :interfaces []
                                     :possibleTypes nil
                                     :enumValues nil
                                     :inputFields nil
                                     :ofType nil})
       ["__Schema" "directives"] (fn [context parent & rest]
                                   [])
       ["__Type" "ofType"] (fn [context parent & rest]
                             (if (:inner-type parent)
                               (introspection/type-resolver (type/get-type-in-schema schema (get-in parent [:inner-type :type-name])))))
       ["__Type" "fields"] (fn [context parent & rest]
                             (map introspection/field-resolver (:fields parent)))
       ["__Type" "enumValues"] (fn [context parent & rest]
                                 (map introspection/enum-resolver (:enumValues parent)))
       ["__Field" "type"] (fn [context parent & rest]
                            (cond
                              (:type-name parent) (introspection/type-resolver (type/get-type-in-schema schema (get parent :type-name)))
                              (:inner-type parent) (introspection/type-resolver parent)
                              :default (throw (ex-info (format "Unhandled type: %s" parent) {}))))
       :else nil))))

(defn create-resolver-fn
  [schema resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn schema)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
