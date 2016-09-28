(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]))

(def default-types
  {"Int"     {:type-name "Int"     :kind :SCALAR}
   "Float"   {:type-name "Float"   :kind :SCALAR}
   "String"  {:type-name "String"  :kind :SCALAR}
   "Boolean" {:type-name "Boolean" :kind :SCALAR}
   "ID"      {:type-name "ID"      :kind :SCALAR}})

(defn inject-introspection-root-query-fields [root-query-type]
  (if root-query-type
    (-> root-query-type
        (assoc-in [:fields "__schema"] {:type-name "__Schema" :required true})
        (assoc-in [:fields "__type"]   {:type-name "__Type"}))))

(defn create-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema introspection-schema]
   (let [definitions (concat (:type-system-definitions parsed-schema)
                             (:type-system-definitions introspection-schema))
         grouped (into {} (group-by :type-system-type definitions))
         sub-grouped (->> (dissoc grouped :schema)
                          (map (fn [[k v]] [k (->> v
                                                   (map #(vector (:type-name %) (dissoc % :type :type-system-type)))
                                                   (into {}))]))
                          (into {})) ;; TODO do most of this in parse
         schemas (:schema grouped)
         schema (or (first schemas) {:kind :SCHEMA :query-type {:name "Query"}})
         root-query-type-name (get-in schema [:query-type :name] "Query")]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:schema     schema
      :types      (-> (into default-types (:type sub-grouped))
                      (assoc-in [root-query-type-name :type-name] root-query-type-name)
                      (assoc-in [root-query-type-name :kind] :OBJECT)
                      (assoc-in [root-query-type-name :fields "__schema"] {:name "__Schema"}))
      :interfaces (get sub-grouped :interface {})
      :unions     (get sub-grouped :union {})
      :inputs     (get sub-grouped :input {})
      :enums      (get sub-grouped :enum {})
      :directives (get sub-grouped :directive {})}))
  ([parsed-schema]
   (create-schema parsed-schema nil)))

(defn inject-introspection-schema [schema introspection-schema]
  "Combine schema definition with introspection schema"
  (-> schema
      (update :types (fn merge-types [types]
                       (-> (merge types (:types introspection-schema))
                           (update (get-in schema [:schema :query-type :name])
                                   inject-introspection-root-query-fields))))
      (update :interfaces (merge (:interfaces introspection-schema)))
      (update :unions (merge (:unions introspection-schema)))
      (update :inputs (merge (:inputs introspection-schema)))
      (update :enums (merge (:enums introspection-schema)))
      (update :directives (merge (:directives introspection-schema)))))

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

(defn get-type-in-schema [schema type-name]
  "Get type definition for given 'type-name' from provided 'schema'."
  (if (nil? type-name)
    (gerror/throw-error "get-type-in-schema: type-name is NULL!"))
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
      (get-type-in-schema schema root-mutation-type-name)
      (gerror/throw-error (format "get-root-mutation-type: schema: '%s' doesn't have root mutation type definition." schema)))))

(defn get-field-type
  "Get the type of a field definied in given 'type-name'."
  [schema type-name field-name]
  (if (nil? type-name)
    (gerror/throw-error (format "get-field-type: type-name is NULL for field(%s)!" field-name)))
  (if (nil? field-name)
    (gerror/throw-error (format "get-field-type: field-name is NULL in type(%s)!" type-name)))
  (let [type (get-type-in-schema schema type-name)
        field-type (get-in type [:fields field-name])
        field-type-kind (:kind field-type)]
    (if (nil? field-type)
      (gerror/throw-error (format "Field (%s) does not exist in type(%s)." field-name type-name)))
    (if field-type-kind
      field-type ; when field type is LIST or NON_NULL
      (get-type-in-schema schema (:type-name field-type)))))

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
  (let [fields (get parent-type :fields)
        field  (get fields field-name)]
    (assert parent-type "Parent type is NULL!")
    (assert field (format "Field(%s) does not exist in parent type %s." field-name parent-type))
    (get-in field [:arguments])))

(comment
  "query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      # subscriptionType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
               type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  }")
