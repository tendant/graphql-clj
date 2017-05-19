(ns graphql-clj.resolver
  (:require [graphql-clj.introspection :as introspection]))

(defn default-resolver-fn [type-name field-name]
  (fn [context parent args]
    (assert type-name (format "type name is NULL for field: %s." field-name))
    (assert field-name (format "field-name is NULL for type: %s." type-name))
    (if (= "__typename" field-name)
      type-name
      (get parent (keyword field-name)))))

(defn get-type-in-schema [schema type-name]
  (assert type-name "type-name is nil!")
  (let [type (symbol type-name)]
    (get-in schema [:type-map type])))

(defn root-type
  [schema root-name]
  (if-let [t (get-type-in-schema schema root-name)]
    (-> (assoc t :type-name (:name t))
        introspection/type-resolver)))

(defn schema-introspection-resolver-fn
  [schema]
  (let [query-root-name (str (or (get-in schema [:roots :query])
                                 "QueryRoot"))
        mutation-root-name (str (get-in schema [:roots :mutation]))]
    (fn [type-name field-name]
      (get-in {query-root-name {"__schema" (fn [context parent args]
                                             {})
                                "__type" (fn [context parent args]
                                           (let [type-name (get args "name")
                                                 type (get-type-in-schema schema type-name)]
                                             (assert type (format "type is nil for type-name: %s." type-name))
                                             (introspection/type-resolver (assoc type :type-name type-name))))}
               "__Schema" {"types" (fn [context parent args]
                                     (introspection/schema-types schema))
                           "queryType" (fn [context  parent args]
                                         (root-type schema query-root-name))
                           "mutationType" (fn [context parent args]
                                            (root-type schema mutation-root-name))
                           "directives" (fn [context parent args]
                                          [])}
               "__Type" {"ofType" (fn [context parent args]
                                    (when-let [inner-type (:inner-type parent)]
                                      (cond
                                        (:required inner-type) (introspection/type-resolver inner-type)
                                        (get-in inner-type [:type-name]) (introspection/type-resolver (get-type-in-schema schema (get-in inner-type [:type-name])))
                                        inner-type (introspection/type-resolver inner-type)
                                        :default (throw (ex-info (format "Unable to process ofType for: %s." parent) {})))))
                         "fields" (fn [context parent args]
                                    (map introspection/field-resolver (:fields parent)))
                         "enumValues" (fn [context parent args]
                                        (map introspection/enum-resolver (:enumValues parent)))
                         "inputFields" (fn [context parent args]
                                         (map introspection/input-field-resolver (:inputFields parent)))}
               "__Field" {"type" (fn [context parent args]
                                   ;; (cond
                                   ;;   (:required parent) (introspection/type-resolver parent)
                                   ;;   (t:inner-ype parent) (introspection/type-resolver parent)
                                   ;;   (:type-name parent) (introspection/type-resolver parent)
                                   ;;   :default (throw (ex-info (format "Unhandled type: %s" parent) {})))
                                   (introspection/type-resolver parent))
                          "args" (fn [context parent args]
                                   (map introspection/args-resolver (:args parent)))}
               "__InputValue" {"type" (fn [context parent args]
                                        (introspection/type-resolver parent))}}
              [type-name field-name])
      )))

(defn create-resolver-fn
  [state resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn state)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))
