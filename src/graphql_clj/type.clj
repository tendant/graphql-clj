(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]))
(defn- type-system-type-filter-fn
  [type]
  (fn [definition]
    (let [type-system-type (:type-system-type definition)]
      (assert type-system-type "type-system-type is NULL!")
      (= type type-system-type))))

(defn- create-type-system-type [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)]
    (assert name "Type definition name is NULL!")
    [name {:name name
           :kind :OBJECT
           :fields type-fields
           :implements (:type-implements definition)}]))

(defn- create-type-system-input [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)]
    (assert name "Input definition name is NULL!")
    [name {:name name
           :kind :INPUT_OBJECT
           :fields type-fields}]))

(defn- create-type-system-union [definition]
  (let [name (:name definition)
        fields (:type-fields definition)]
    (assert name "Union definition name is NULL!")
    [name {:name name
           :kind :UNION
           :fields fields}]))

(defn- create-type-system-interface [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)]
    (assert name "Interface definition name is NULL!")
    [name {:name name
           :kind :INTERFACE
           :fields type-fields}]))

(defn- create-type-system-enum [definition]
  (let [name (:name definition)
        enum-fields (:enum-fields definition)]
    (assert name "Enum definition name is NULL!")
    [name {:name name
           :kind :ENUM
           :fields enum-fields}]))

(defn- create-type-system-directive [definition]
  (let [name (:name definition)
        on (:directive-on-name definition)]
    (assert name "Directive definition name is NULL!")
    [name {:name name
           :kind :DIRECTIVE
           :on on}]))

(defn- create-type-system-schema [definition]
  (let [schema-types (:schema-types definition)]
    (assert schema-types "schema-types is NULL!")
    (merge {:kind :SCHEMA}
           schema-types)))

(defn- create-type-system-definition [definition]
  (let [type (:type-system-type definition)]
    (assert type "type system type is NULL!")
    (case type
      :type  (create-type-system-type definition)
      :input (create-type-system-input definition)
      :union  (create-type-system-union definition)
      :interface (create-type-system-interface definition)
      :enum (create-type-system-enum definition)
      :directive (create-type-system-directive definition)
      :schema (create-type-system-schema definition))))

(defn- type-system-type-definitions
  [type]
  (fn [definitions]
    (->> definitions
         (filter (type-system-type-filter-fn type))
         (map create-type-system-definition))))

(def default-types
  {"Int" {:name "Int"
          :kind :SCALAR}
   "Float" {:name "Float"
            :kind :SCALAR}
   "String" {:name "String"
             :kind :SCALAR}
   "Boolean" {:name "Boolean"
              :kind :SCALAR}})

(defn inject-introspection-root-query-fields [root-query-type]
  (if root-query-type
    (-> root-query-type
        (assoc-in [:fields "__schema"] {:name "__Schema" :required true})
        (assoc-in [:fields "__type"]   {:name "__Type"}))))

(defn create-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema introspection-schema]
   (let [definitions (concat (:type-system-definitions parsed-schema)
                             (:type-system-definitions introspection-schema))
         types ((type-system-type-definitions :type) definitions)
         interfaces ((type-system-type-definitions :interface) definitions)
         unions ((type-system-type-definitions :union) definitions)
         inputs ((type-system-type-definitions :input) definitions)
         enums ((type-system-type-definitions :enum) definitions)
         directives ((type-system-type-definitions :directive) definitions)
         schemas ((type-system-type-definitions :schema) definitions) ; validate only one schema has been defined
         schema (first schemas)
         root-query-type-name (or (get-in schema [:query-type :name])
                                  "Query")
         ]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:schema     (or schema
                      ;; Default schema
                      {:kind       :SCHEMA
                       :query-type {:name "Query"}})
      :types      (-> (into default-types types)
                      (assoc-in [root-query-type-name :name] root-query-type-name)
                      (assoc-in [root-query-type-name :kind] :OBJECT)
                      (assoc-in [root-query-type-name :fields "__schema"] {:name "__Schema"}))
      :interfaces (into {} interfaces)
      :unions     (into {} unions)
      :inputs     (into {} inputs)
      :enums      (into {} enums)
      :directives (into {} directives)}))
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
      (get-type-in-schema schema (:name field-type)))))

(defn get-inner-type
  "Get inner type of 'field-type'"
  [schema field-type]
  (let [inner-type (:inner-type field-type)
        inner-type-kind (:kind inner-type)]
    (if inner-type-kind
      inner-type
      (if-let [inner-type-name (:name inner-type)]
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
