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
  (let [type-symbol (symbol type-name)
        type (get-in schema [:type-map type-symbol])]
    type))

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
                                             ;; (assert type (format "type is nil for type-name: %s." type-name))
                                             (if type
                                               (introspection/type-resolver (assoc type :type-name type-name)))))}
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
                                        :default (throw (ex-info (format "Unable to process ofType for: %s." parent) {})))))}
               "__Field" {"name" (fn [context parent args]
                                   (:name parent))
                          "description" (fn [context parent args]
                                          (:description parent))
                          "args" (fn [context parent args]
                                   (or (:arguments parent)
                                       []))
                          "type" (fn [context parent args]
                                   (introspection/type-resolver (:type parent))
                                   ;; (cond
                                   ;;   (:required parent) (introspection/type-resolver parent)
                                   ;;   (t:inner-ype parent) (introspection/type-resolver parent)
                                   ;;   (:type-name parent) (introspection/type-resolver parent)
                                   ;;   :default (throw (ex-info (format "Unhandled type: %s" parent) {})))
                                   )
                          "isDeprecated" (fn [context parent args]
                                           ;; TODO
                                           false)
                          "deprecationReason" (fn [context parent args]
                                                ;; TODO
                                                nil)
                          ;; "args" (fn [context parent args]
                          ;;          (map introspection/args-resolver (:args parent)))
                          }
               "__InputValue" {"name" (fn [context parent args]
                                        (:name parent))
                               "description" (fn [context parent args]
                                               (:description parent))
                               "type" (fn [context parent args]
                                        (introspection/type-resolver (:type parent)))
                               "defaultValue" (fn [context parent args]
                                                (get-in parent [:default-value :value]))}
               "__EnumValue" {"name" (fn [context parent args]
                                       (:name parent))
                              "description" (fn [context parent args]
                                              (introspection/filter-comment (:doc parent)))
                              "isDeprecated" (fn [context parent args]
                                               ;; TODO
                                               false)
                              "deprecationReason" (fn [context parent args]
                                                    ;; TODO
                                                    nil)}
               "__Directive" {"name" (fn [context parent args]
                                       (:name parent))
                              "description" (fn [context parent args]
                                              (:description parent))
                              "locations" (fn [context parent args]
                                            ;; TODO
                                            nil)
                              "args" (fn [context parent args]
                                       ;; TODO
                                       nil)}}
              [type-name field-name])
      )))

(defn create-resolver-fn
  [state resolver-fn]
  (let [schema-resolver-fn (schema-introspection-resolver-fn state)]
    (fn [type-name field-name]
      (or (and resolver-fn (resolver-fn type-name field-name))
          (schema-resolver-fn type-name field-name)
          (default-resolver-fn type-name field-name)))))

;; (def intro-str "query {__schema { types {name}}}")
