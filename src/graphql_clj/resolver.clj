(ns graphql-clj.resolver
  (:require [clojure.core.match :as match]
            [graphql-clj.type :as type]))

(defn default-resolver-fn [type-name field-name]
  (fn [context parent & args]
    (assert type-name (format "type name is NULL for field: %s." field-name))
    (assert field-name (format "field-name is NULL for tyep: %s." type-name))
    (get parent (keyword field-name))))

(defn- get-root-query-type-name [schema]
  (-> schema type/get-root-query-type :fields first :type-name))

(defn schema-introspection-resolver-fn
  [schema]
  (let [root-query-name (get-root-query-type-name schema)]
    (fn [type-name field-name]
      (match/match
       [type-name field-name]
       [root-query-name "__schema"] (fn [context parent & args]
                                      {:types (concat (vals (:types schema))
                                                      ;; Work around for graphiql to treat interface as type.
                                                      (vals (:interfaces schema))
                                                      ;; Work around for graphiql to treat enum as type.
                                                      (vals (:enums schema)))
                                       :queryType (type/get-type-in-schema schema root-query-name)
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
       ["__Type" "fields"] (fn [context parent & rest]
                             (let [kind (:kind parent)]
                               (case kind
                                 (:OBJECT :INTERFACE) (:fields parent)
                                 [])))
       ["__Type" "interfaces"] (fn [context parent & rest]
                                 (let [implements (get-in parent [:implements :type-names])]
                                   (map (fn type-interfaces [interface-name]
                                          (type/get-interface-in-schema schema interface-name))
                                        implements)))
       ["__Type" "possibleTypes"] (fn [& rest]
                                    [])
       ["__Type" "enumValues"] (fn [context parent & rest]
                                 (if (and (= :ENUM (:kind parent)) (:fields parent))
                                   (vals (:fields parent))
                                   []))
       ["__Type" "inputFields"] (fn [& rest]
                                  [])
       ["__Type" "ofType"] (fn [context parent & rest]
                             (if (:inner-type parent)
                               (type/get-type-in-schema schema (get-in parent [:inner-type :type-name]))))
       ["__Field" "name"] (fn [context parent & rest]
                            (let [[name _] parent]
                              name))
       ["__Field" "type"] (fn [context parent & rest]
                            (let [[_ type] parent
                                  field-type (get-in type [:type-field-type]) ;; TODO probably wrong, as :type-field-type no longer appears in parsed tree
                                  type-name (:type-name field-type)]
                              (if (:kind field-type)
                                field-type
                                (type/get-type-in-schema schema type-name))))
       ["__Field" "args"] (fn [& rest]
                            [])
       ["__EnumValue" "name"] (fn [context parent & rest]
                                (:name parent))             ;; TODO probably wrong, could be :type-name
       :else nil))))

(defn create-resolver-fn
  [schema resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn schema)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
