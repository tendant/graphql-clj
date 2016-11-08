(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]
            [graphql-clj.box :as box]))

(def default-types
  {"Int"     {:type-name "Int"     :kind :SCALAR}
   "Float"   {:type-name "Float"   :kind :SCALAR}
   "String"  {:type-name "String"  :kind :SCALAR}
   "Boolean" {:type-name "Boolean" :kind :SCALAR}
   "ID"      {:type-name "ID"      :kind :SCALAR}})

(defn get-enum-in-schema [schema enum-name]
  "Get enum definition for given 'enum-name' from provided 'schema'."
  (if (nil? enum-name)
    (gerror/throw-error "get-enum-in-schema: enum-name is NULL!"))
  (get-in schema [:enums (name enum-name)]))

(defn get-interface-in-schema [schema interface-name]
  "Get interface definition for given 'interface-name' from provided 'schema'."
  (if (nil? interface-name)
    (gerror/throw-error "get-interface-in-schema: interface-name is NULL!"))
  (get-in schema [:interfaces (name interface-name)]))

(defn get-type-in-schema
  "Get type definition for given 'type-name' from provided 'schema'."
  [schema type-name]
  (when (nil? type-name) (gerror/throw-error "get-type-in-schema: type-name is NULL!"))
  (or (get-in schema [:types (name type-name)])
      ;; type could be enum
      (get-enum-in-schema schema (name type-name))
      ;; TODO: type could be interface, Should also check type implments interface
      (get-interface-in-schema schema (name type-name))))

(defn get-root-query-type
  "Get root query type name from schema definition."
  [schema]
  (let [root-query-type-name (get-in schema [:schema :query-type :name])]
    (if root-query-type-name
      (get-type-in-schema schema root-query-type-name)
      (gerror/throw-error (format "get-root-query-type: schema: '%s' doesn't have root query type definition." schema)))))

(defn get-root-mutation-type
  "Get root mutation type name from schema definition."
  [schema]
  (let [root-mutation-type-name (get-in schema [:schema :mutation-type :name])]
    (if root-mutation-type-name
      (get-type-in-schema schema root-mutation-type-name))))

(defn type->field
  "Get the field definition for a specific field on an object type"
  [type field-name]
  (->> type :fields (filter #(= (name field-name) (:field-name %))) first))

(defn get-field-type
  "Get the type of a field defined in given 'type-name'."
  [schema type-name field-name]
  (if (nil? type-name)
    (gerror/throw-error (format "get-field-type: type-name is NULL for field(%s)!" field-name)))
  (if (nil? field-name)
    (gerror/throw-error (format "get-field-type: field-name is NULL in type(%s)!" type-name)))
  (let [type (get-type-in-schema schema type-name)
        {:keys [kind] :as field-type} (type->field type field-name)]
    (when (nil? field-type) (gerror/throw-error (format "Field (%s) does not exist in type(%s)." field-name type-name)))
    (if (nil? field-type)
      (gerror/throw-error (format "Field (%s) does not exist in type(%s)." field-name type-name)))
    (if (= :LIST kind)
      field-type
      (do (assert (:type-name field-type) (format "Field (%s) has no type-name."  field-type))
          (get-type-in-schema schema (:type-name field-type))))))

(defn get-inner-type
  "Get inner type of 'field-type'"
  [schema field-type]
  (let [inner-type (:inner-type field-type)
        inner-type-kind (:kind inner-type)]
    (if inner-type-kind
      inner-type
      (if-let [inner-type-name (:type-name inner-type)]
        (get-type-in-schema schema inner-type-name)
        (gerror/throw-error (format "get-inner-type: failed getting inner type of field-type(%s)" field-type))))))

(defn get-field-arguments
  [parent-type field-name]
  (let [field  (type->field parent-type (name field-name))]
    (assert parent-type "Parent type is NULL!")
    (assert field (format "Field(%s) does not exist in parent type %s." field-name parent-type))
    (:arguments field)))

(defn get-arguments-default-value-map
  [arguments]
  (when arguments
    (reduce (fn [result argument]
              (if (:default-value argument)
                (assoc result (name (:argument-name argument)) (box/box->val (:default-value argument)))
                result))
            {} arguments)))

