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

(defn filter-comment
  [comment]
  (if comment
    (-> comment
        (clojure.string/replace #"^\w*#\W*" "")
        (clojure.string/replace #"\r?\n\w*#\w*" "\n"))))

(defn type-kind [type]
  (if (:kind type)
    (:kind type)
    (do
      (assert (:tag type) (format "type tag is nil for type: %s" type))
      (let [tag (:tag type)
            name (:name type)]
        (cond
          (= :list-type tag) :LIST ; has to be before :NON_NULL
          (:required type) :NON_NULL
          (and (= :basic-type tag)
               (contains? #{'Int 'Float 'String 'Boolean 'ID} name)) :SCALAR
          (= :basic-type tag) :OBJECT
          (= :type-definition tag) :OBJECT ; root query kind
          (= :union-definition tag) :UNION
          (= :interface-definition tag) :INTERFACE
          (= :enum-definition tag) :ENUM
          (= :input-definition tag) :INPUT_OBJECT
          (= :scalar-definition tag) :SCALAR
          :else (assert nil (format "Unhandled type kind for type: %s" type)))))))

(defn field-kind [field]
  (let [tag (:tag field)
        name (:name field)
        type (:type field)]
    (assert tag (format "field tag is nil for type: %s" field))
    (assert name (format "field name is nil for type: %s" field))
    (cond
      (= :type-field tag) (type-kind type)
      :else (throw (ex-info (format "unknown field kind for field: %s" field) {:field field})))))

(defn field-resolver [field]
  (assert (:name field)
          (format "field name is null for field: %s" field))
  (assert (:tag field)
          (format "field tag is null for field: %s" field))
  (let [type-name (or (:type-name field)
                      (get-in field [:type :name]))
        type (:type field)
        kind (field-kind field)
        inner-type (or (get-in field [:type :inner-type])
                       (if (= :NON_NULL kind)
                         (dissoc type :required)))]
    (assert kind (format "field kind is nil for field: %s" field))
    (if (#{:LIST} kind)
      (assert inner-type (format "field inner-type is nil for field: %s" field)))
    (assert (or type-name inner-type) (format "field type-name and inner-type are both nil for field: %s" field))
    {:name (:name field)
     :description (filter-comment (:doc field))
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
  (let [kind (type-kind type)
        type-name (or (:type-name type)
                      (if (:tag type)
                        (case (:tag type)
                          :basic-type (:name type)
                          :list-type nil
                          (throw (ex-info (format "Unhandled type in type-resolver:%s" type) {:type type})))))
        inner-type (:inner-type type)]
    (when (not (#{:LIST :NON_NULL} kind))
      (assert type-name (format "type-name is null for type: %s." type))
      (assert kind (format "kind is nil for type: %s" type)))
    (if (#{:LIST} kind)
      (assert inner-type (format "inner-type is nil for type: %s" type)))
    (assert (or type-name inner-type) (format "Both type-name and inner-type are nil for type:%s" type))
    (if (:required type)
      ;; Wrap required type ast NON_NULL
      {;; A Non-Null type cannot modify another Non-Null type.
       :kind :NON_NULL
       ;; defer resolving ofType
       :inner-type (dissoc type :required)}
      {:kind kind
       :name type-name
       :description (filter-comment (:doc type))

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
                     (:constants type))

       ;; INPUT_OBJECT only
       :inputFields (when (= :INPUT_OBJECT kind)
                      (assert (:fields type) (format "input fields is empty for type: %s." type))
                      (:fields type))

       ;; NON_NULL and LIST only
       ;; :ofType nil
       :inner-type inner-type ; provide inner-type for ofType resolver to avoid recursive calls
       })))

(defn schema-types [schema]
  (assert (map? (:type-map schema)) (format "schema has no :type-map: %s" schema))
  (let [types (->> (vals (:type-map schema))
                   (map (fn [t]
                          (assert (:name t) (format "type doesn't have a name: %s" t))
                          (assoc t :type-name (:name t))))
                   (map type-resolver))]
    types))

(defn input-field-resolver [field]
  (let [name (:name field)
        type (:type field)
        type-name (:name type)
        kind (type-kind type)
        inner-type (:inner-type field)]
    (assert name (format "field name is null for input value: %s." field))
    (assert type (format "field type is nil for input field: %s" field))
    (when (#{:LIST :NON_NULL} kind)
      (assert inner-type (format "inner-type is nil for type: %s" type)))
    (assert (or type-name inner-type) (format "Both type-name and inner-type are nil for input field:%s" field))
    {:name name
     :description (filter-comment (:doc field))

     :type-name type-name
     :kind kind
     :inner-type inner-type
     :required (:required field)

     :default-value (:default-value field)}))

(defn enum-resolver [enum]
  (assert (:name enum) (format "enum name is null for:%s" enum))
  {:name (:name enum)
   :description (filter-comment (:doc enum))
   :isDeprecated false ; TODO
   :deprecationReason nil ; TODO
   })

(defn args-resolver [arg]
  (assert (:name arg) (format "argument name is null for:%s" arg))
  (assert (:type arg) (format "argument type is nil for argument:%s" arg))
  (let [type (:type arg)
        type-name (:name type)
        kind (type-kind (:type arg))
        inner-type (or (get-in arg [:type :inner-type])
                       (if (= :NON_NULL kind)
                         (dissoc type :required)))]
    (assert kind (format "argument kind is nil for: %s" arg))
    (when (#{:LIST} kind)
      (assert inner-type (format "inner-type is nil for type: %s" arg)))
    (assert (or type-name inner-type) (format "Both type-name and inner-type are nil for arg:%s" arg))
    {:name (:name arg)
     :description (filter-comment (:doc arg))
     
     ;; defer resolving of type for argument
     :type-name type-name
     :kind kind
     :inner-type inner-type
     :required (:required arg)
     
     :defaultValue (:default-value arg)}))
