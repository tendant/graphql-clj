(ns graphql-clj.resolver
  (:require [clojure.core.match :as match]
            [graphql-clj.type :as type]
            [graphql-clj.introspection :as introspection]))

(defn default-resolver-fn [type-name field-name]
  (fn [context parent args]
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
       [root-query-name "__schema"] (fn [context parent args]
                                      (let [mutation (type/get-root-mutation-type schema)]
                                        {:types (introspection/schema-types schema)
                                         :queryType (introspection/type-resolver (type/get-root-query-type schema))
                                         :mutationType (if mutation
                                                         (introspection/type-resolver mutation))
                                         :directives []}))
       [root-query-name "__type"] (fn [context parent args]
                                    (let [type-name (get args "name")
                                          type (type/get-type-in-schema schema type-name)]
                                      (introspection/type-resolver type)))
       ["__Schema" "directives"] (fn [context parent args]
                                   [])
       ["__Type" "ofType"] (fn [context parent args]
                             (when-let [inner-type (:inner-type parent)]
                               (cond
                                 (:required inner-type) (introspection/type-resolver inner-type)
                                 (get-in inner-type [:type-name]) (introspection/type-resolver (type/get-type-in-schema schema (get-in inner-type [:type-name])))
                                 inner-type (introspection/type-resolver inner-type)
                                 :default (throw (ex-info (format "Unable to process ofType for: %s." parent) {})))))
       ["__Type" "fields"] (fn [context parent args]
                             (map introspection/field-resolver (:fields parent)))
       ["__Type" "enumValues"] (fn [context parent args]
                                 (map introspection/enum-resolver (:enumValues parent)))
       ["__Type" "inputFields"] (fn [context parent args]
                                  (map introspection/input-field-resolver (:inputFields parent)))
       ["__Field" "type"] (fn [context parent args]
                            (cond
                              (:required parent) (introspection/type-resolver parent)
                              (:inner-type parent) (introspection/type-resolver parent)
                              (:type-name parent) (introspection/type-resolver (type/get-type-in-schema schema (get parent :type-name)))
                              :default (throw (ex-info (format "Unhandled type: %s" parent) {}))))
       ["__Field" "args"] (fn [context parent args]
                           (map introspection/args-resolver (:args parent)))
       ["__InputValue" "type"] (fn [context parent args]
                                 (introspection/type-resolver parent))
       :else nil))))

(defn create-resolver-fn
  [schema resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn schema)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
