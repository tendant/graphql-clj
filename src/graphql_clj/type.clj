(ns graphql-clj.type
  (:require [instaparse.core :as insta]
            [graphql-clj.error :as gerror]
            [graphql-clj.introspection :as intro]))

(def default-types
  {"Int"     {:type-name "Int"     :kind :SCALAR :pred int?}
   "Float"   {:type-name "Float"   :kind :SCALAR :pred double?}
   "String"  {:type-name "String"  :kind :SCALAR :pred string?}
   "Boolean" {:type-name "Boolean" :kind :SCALAR :pred boolean?}
   "ID"      {:type-name "ID"      :kind :SCALAR :pred string?}})

(def default-type-names (set (keys default-types)))

(defn query-root-name
  "Given a parsed schema document, return the query-root-name (default is Query)"
  [parsed-schema]                           ;; TODO deduplicate, TODO test
  (or (some->> parsed-schema
               :type-system-definitions
               (filter #(= :schema-definition (:node-type %)))
               first
               :query-type
               :name) "Query"))

(defn query-root-fields
  "Given a parsed schema document, return [query-root-name {root-field Type}]
   When validating queries, we need to know the types of the root fields to map to the same specs
   that we registered when parsing the schema."
  [root-query-name parsed-schema]                           ;; TODO deduplicate, TODO test
  (some->> parsed-schema
           :type-system-definitions
           (filter #(= (:type-name %) root-query-name))
           first
           :fields
           (map (juxt :field-name :type-name))
           (into {})))

(defn create-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema introspection-schema]
   (assert (not (insta/failure? parsed-schema)) (format "Schema is invalid (%s)." parsed-schema))
   (assert (not (insta/failure? introspection-schema)) (format "Introspection Schema is invalid (%s)." introspection-schema))
   (let [definitions (concat (:type-system-definitions parsed-schema)
                             (:type-system-definitions introspection-schema))
         grouped (into {} (group-by :node-type definitions))
         sub-grouped (->> (dissoc grouped :schema-definition)
                          (map (fn [[k v]] [k (->> (map #(vector (:type-name %) (dissoc % :type)) v) (into {}))]))
                          (into {}))
         schemas (:schema-definition grouped)
         schema (or (first schemas) {:node-type :schema-definition :query-type {:name "Query"}})
         root-query-type-name (get-in schema [:query-type :name])]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:schema     schema
      :types      (-> (into default-types (:type-definition sub-grouped))
                      (update-in [root-query-type-name] intro/upsert-root-query root-query-type-name))
      :interfaces (get sub-grouped :interface-definition {})
      :unions     (get sub-grouped :union-definition {})
      :inputs     (get sub-grouped :input-definition {})
      :enums      (get sub-grouped :enum-definition {})
      :directives (get sub-grouped :directive-definition {})}))
  ([parsed-schema]
   (create-schema parsed-schema nil)))

(defn get-enum-in-schema [schema enum-name]
  "Get enum definition for given 'enum-name' from provided 'schema'."
  (if (nil? enum-name)
    (gerror/throw-error "get-enum-in-schema: enum-name is NULL!"))
  (get-in schema [:enums enum-name]))

(defn get-interface-in-schema [schema interface-name]
  "Get interface definition for given 'interface-name' from provided 'schema'."
  (if (nil? interface-name)
    (gerror/throw-error "get-interface-in-schema: interface-name is NULL!"))
  (get-in schema [:interfaces interface-name]))

(defn get-type-in-schema
  "Get type definition for given 'type-name' from provided 'schema'."
  [schema type-name]
  (when (nil? type-name) (gerror/throw-error "get-type-in-schema: type-name is NULL!"))
  (or (get-in schema [:types type-name])
      ;; type could be enum
      (get-enum-in-schema schema type-name)
      ;; TODO: type could be interface, Should also check type implments interface
      (get-interface-in-schema schema type-name)))

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
  (->> type :fields (filter #(= field-name (:field-name %))) first))

(defn get-field-type
  "Get the type of a field defined in given 'type-name'."
  [schema type-name field-name]
  (if (nil? type-name)
    (gerror/throw-error (format "get-field-type: type-name is NULL for field(%s)!" field-name)))
  (if (nil? field-name)
    (gerror/throw-error (format "get-field-type: field-name is NULL in type(%s)!" type-name)))
  (let [type (get-type-in-schema schema type-name)
        {:keys [node-type] :as field-type} (type->field type field-name)]
    (when (nil? field-type) (gerror/throw-error (format "Field (%s) does not exist in type(%s)." field-name type-name)))
    (if (nil? field-type)
      (gerror/throw-error (format "Field (%s) does not exist in type(%s)." field-name type-name)))
    (if (= :list node-type)
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
  (let [field  (type->field parent-type field-name)]
    (assert parent-type "Parent type is NULL!")
    (assert field (format "Field(%s) does not exist in parent type %s." field-name parent-type))
    (:arguments field)))

(defn get-arguments-default-value-map
  [arguments]
  (when arguments
    (reduce (fn [result argument]
              (if (:default-value argument)
                (assoc result (:argument-name argument) (:default-value argument))
                result))
            {} arguments)))

