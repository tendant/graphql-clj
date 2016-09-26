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
                                      {:types (vals (:types schema))
                                       :queryType (type/get-type-in-schema schema root-query-name)
                                       :mutationType nil
                                       :directives []})
       [root-query-name "__type"] (fn [context parent & args]
                                    {:kind nil
                                     :name nil
                                     :description nil
                                     :fields []
                                     :interfaces []
                                     :possibleTypes nil
                                     :enumValues nil
                                     :inputFields nil
                                     :ofType nil})
       ["__Schema" "types"] (fn [context parent & rest]
                              (vals (:types schema)))
       ["__Schema" "directives"] (fn [context parent & rest]
                                   [])
       ["__Type" "fields"] (fn [context parent & rest]
                             (:fields parent))
       ["__Type" "interfaces"] (fn [context parent & rest]
                                 (println "interfaces parent: " parent)
                                 (let [implements (get-in parent [:implements :type-names])]
                                   (map (fn type-interfaces [interface-name]
                                          (type/get-interface-in-schema schema interface-name))
                                        implements)))
       ["__Type" "possibleTypes"] (fn [& rest]
                                    [])
       ["__Type" "enumValues"] (fn [& rest]
                                 [])
       ["__Type" "inputFields"] (fn [& rest]
                                  [{:name "test"
                                    :type (type/get-type-in-schema schema root-query-name)}])
       ["__Field" "name"] (fn [context parent & rest]
                            (let [[name _] parent]
                              name))
       ["__Field" "type"] (fn [context parent & rest]
                            (let [[_ type] parent
                                  field-type (get-in type [:type-field-type])
                                  type-name (:name field-type)]
                              (if (:kind field-type)
                                field-type
                                (type/get-type-in-schema schema type-name))))
       ["__Field" "args"] (fn [& rest]
                            [])
       ["__Type" "ofType"] (fn [context parent & rest]
                             (if (:inner-type parent)
                               (type/get-type-in-schema schema (get-in parent [:inner-type :type-field-type :name]))))
       :else nil))))

(defn create-resolver-fn
  [schema resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn schema)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
