(ns graphql-clj.introspection
  (:require [graphql-clj.parser :as parser]
            [clojure.java.io :as io]))

(def introspection-schema-str (slurp (io/resource "introspection.schema")))

(def introspection-schema (parser/parse introspection-schema-str))

(def introspection-query (slurp (io/resource "introspection.graphql")))

;; (parser/parse "type __Schema {
;;   __schema: __Schema!
;;   __type(name: String!): __Type
;; }")
;; https://facebook.github.io/graphql/#sec-Schema-Introspection
;; The schema introspection system is accessible from the meta‐fields __schema and __type which are
;; accessible from the type of the root of a query operation. These fields are implicit and do not
;; appear in the fields list in the root type of the query operation.
(def root-query-schema-fields
  [{:field-name "__schema" :type-name "__Schema" :node-type :type-field :required true}
   {:field-name "__type" :type-name "__Type" :node-type :type-field
    :arguments [{:node-type :type-field-argument
                 :argument-name "name"
                 :type-name "String"
                 :required true}]}])

(defn- default-root-query-node [root-query-name]
  {:node-type :type-definition
   :type-name root-query-name
   :section   :type-system-definitions
   :fields    root-query-schema-fields
   :kind      :OBJECT})

(defn upsert-root-query [root-query-node root-query-name]
  (if root-query-node
    (update root-query-node :fields into root-query-schema-fields)
    (default-root-query-node root-query-name)))

(defn type-resolver [type]
  (when (not (contains? #{:LIST :NON_NULL} (:kind type)))
    (assert (:type-name type) (format "type name is null for type: %s." type)))
  (if (:required type)
    ;; Wrap required type ast NON_NULL
    {;; A Non-Null type cannot modify another Non-Null type.
     :kind :NON_NULL
     ;; defer resolving ofType
     :inner-type (dissoc type :required)}
    {:kind (:kind type)
     :name (:type-name type)
     :description (:description type)

     ;; OBJECT and INTERFACE only
     :fields (when (contains? #{:OBJECT :INTERFACE} (:kind type))
               (:fields type)) ; defer resolving of fields

     ;; OBJECT only
     :interfaces [] ; TODO

     ;; INTERFACE and UNION only
     :possibleTypes nil

     ;; ENUM only
     :enumValues (when (= :ENUM (:kind type))
                   (assert (:fields type) (format "enum values is empty for type: %s." type))
                   (:fields type))

     ;; INPUT_OBJECT only
     :inputFields (when (= :INPUT_OBJECT (:kind type))
                    (assert (:fields type) (format "input fields is empty for type: %s." type))
                    (:fields type))

     ;; NON_NULL and LIST only
     ;; :ofType nil
     :inner-type (:inner-type type) ; provide inner-type for ofType resolver
     }))

(defn schema-types [schema]
  (->> (concat (vals (:types schema))
               (vals (:interfaces schema))
               (vals (:enums schema))
               (vals (:inputs schema)))
       (map type-resolver)))

(defn field-resolver [field]
  (assert (:field-name field) (format "field name is null for field: %s." field))
  {:name (:field-name field)
   :description (:description field)
   :args (:arguments field) ; defer resolving of arguments
   
   ;; :type nil ; defer resolving of type
   :type-name (:type-name field)
   :kind (:kind field)
   :inner-type (:inner-type field)
   :required (:required field)
   
   :isDeprecated false ; TODO
   :deprecationReason nil ; TODO
   })

(defn input-field-resolver [field]
  (assert (:field-name field) (format "field name is null for input value: %s." field))
  {:name (:field-name field)
   :description (:description field)

   :type-name (:type-name field)
   :kind (:kind field)
   :inner-type (:inner-type field)
   :required (:required field)

   :default-value (:default-value field)})

(defn enum-resolver [enum]
  {:name (:name enum)
   :description nil
   :isDeprecated nil
   :deprecationReason nil})

(defn args-resolver [arg]
  {:name (:argument-name arg)
   :description (:description arg)
   
   ;; defer resolving of type for argument
   :type-name (:type-name arg)
   :kind (:kind arg)
   :inner-type (:inner-type arg)
   :required (:required arg)
   
   :defaultValue (:default-value arg)})
