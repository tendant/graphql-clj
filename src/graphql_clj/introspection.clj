(ns graphql-clj.introspection
  (:require [graphql-clj.parser :as parser]
            [clojure.java.io :as io]))

(def introspection-schema-str (slurp (io/resource "introspection.schema")))

(def introspection-schema (parser/parse-schema introspection-schema-str))

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
  [{:field-name "__schema" :type-name '__Schema :node-type :type-field :required true}
   {:field-name "__type" :type-name '__Type :node-type :type-field
    :arguments [{:node-type :type-field-argument
                 :argument-name 'name
                 :type-name "String"
                 :required true}]
    :arg-map {'name {:node-type :type-field-argument
                     :argument-name 'name
                     :type-name "String"
                     :required true}}}])

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

(defn type-kind [type]
  (if (:kind type)
    (:kind type)
    (do
      (assert (:tag type) (format "type tag is nil for type: %s" type))
      (let [tag (:tag type)
            name (:name type)]
        (cond
          (:required type) :NON_NULL
          (and (= :basic-type tag)
               (contains? #{'Int 'Float 'String 'Boolean 'ID} name)) :SCALAR
          (= :basic-type tag) :OBJECT
          (= :list-type tag) :LIST
          (= :type-definition tag) :OBJECT ; root query kind
          (= :union-definition tag) :UNION
          (= :interface-definition tag) :INTERFACE
          (= :enum-definition tag) :ENUM
          (= :scalar-definition tag) :SCALAR
          :else (assert nil (format "unknown type kind for type: %s" type)))))))

(defn field-kind [field]
  (assert (:tag field) (format "field tag is nil for type: %s" field))
  (assert (:name field) (format "field name is nil for type: %s" field))
  (let [tag (:tag field)
        name (:name field)
        type (:type field)]
    (cond
      (= :type-field tag) (type-kind type)
      :else (throw (ex-info (format "unknown field kind for field: %s" field) {:field field})))))

(defn field-resolver [field]
  (assert (:name field)
          (format "field name is null for field: %s" field))
  (assert (:tag field)
          (format "field tag is null for field: %s" field))
  (let [type-name (or (:type-name field) (get-in field [:type :name]))
        kind (field-kind field)
        inner-type (get-in field [:type :inner-type])]
    (assert kind (format "field kind is nil for field: %s" field))
    (assert (or type-name inner-type) (format "field type-name and inner-type are both nil for field: %s" field))
    {:name (:name field)
     :description (:description field)
     :args (:arguments field) ; defer resolving of arguments
     
     ;; :type nil ; defer resolving of type
     :type-name type-name
     :kind kind
     :inner-type inner-type
     :required (:required field)
     
     :isDeprecated false ; TODO
     :deprecationReason nil ; TODO
     }))

(defn type-resolver [type]
  (let [kind (type-kind type)]
    (when (not (contains? #{:LIST :NON_NULL} kind))
      (assert (or (:name type) (:type-name type)) (format "type name is null for type: %s." type))
      (assert kind (format "kind is nil for type: %s" type)))
    (if (:required type)
      ;; Wrap required type ast NON_NULL
      {;; A Non-Null type cannot modify another Non-Null type.
       :kind :NON_NULL
       ;; defer resolving ofType
       :inner-type (dissoc type :required)}
      {:kind kind
       :name (:name type)
       :description (:description type)

       ;; OBJECT and INTERFACE only
       :fields (when (contains? #{:OBJECT :INTERFACE} kind)
                 (:fields type)) ; defer resolving of fields

       ;; OBJECT only
       :interfaces [] ; TODO

       ;; INTERFACE and UNION only
       :possibleTypes nil

       ;; ENUM only
       :enumValues (when (= :ENUM kind)
                     (assert (:constants type) (format "enum constants is empty for type: %s." type))
                     (:fields type))

       ;; INPUT_OBJECT only
       :inputFields (when (= :INPUT_OBJECT kind)
                      (assert (:fields type) (format "input fields is empty for type: %s." type))
                      (:fields type))

       ;; NON_NULL and LIST only
       ;; :ofType nil
       :inner-type (:inner-type type) ; provide inner-type for ofType resolver
       })))

(defn schema-types [schema]
  (assert (map? (:type-map schema)) (format "schema has no :type-map: %s" schema))
  (map type-resolver (vals (:type-map schema))))

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
  (assert (:name enum) (format "enum name is null for:%s" enum))
  {:name (:name enum)
   :description nil
   :isDeprecated false
   :deprecationReason nil})

(defn args-resolver [arg]
  (assert (:name arg) (format "argument-name is null for:%s" arg))
  (let [type-name (or (:type-name arg) (get-in arg [:type :name]))
        kind (or (:kind arg) (type-kind (:type arg)))]
    (assert kind (format "argument kind is nil for: %s" arg))
    (assert type-name (format "argument type name is nil for: %s" arg))
    {:name (:name arg)
     :description (:description arg)
     
     ;; defer resolving of type for argument
     :type-name type-name
     :kind kind
     :inner-type (:inner-type arg)
     :required (:required arg)
     
     :defaultValue (:default-value arg)}))
