(ns graphql-clj.type
  (:require [taoensso.timbre :as log]
            [clojure.java.io :as io]))

(defn- type-system-type-filter-fn
  [type]
  (fn [definition]
    (= type (:type-system-type definition))))

(defn- create-type-system-fields [fields]
  (log/debug "create-type-system-fields: fields: " fields)
  (->> fields
       (map (fn create-type-system-fields-convert-field [field]
              [(:name field) field]))
       (into {})))

(defn- create-type-system-type [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)
        fields (create-type-system-fields type-fields)]
    [name {:name name
           :kind :OBJECT
           :fields fields}]))

(defn- create-type-system-input [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)
        fields (create-type-system-fields type-fields)]
    [name {:name name
           :kind :INPUT_OBJECT
           :fields fields}]))

(defn- create-type-system-union [definition]
  (let [name (:name definition)
        fields (:type-fields definition)]
    [name {:name name
           :kind :UNION
           :fields fields}]))

(defn- create-type-system-interface [definition]
  (let [name (:name definition)
        type-fields (:type-fields definition)
        fields (create-type-system-fields type-fields)]
    [name {:name name
           :kind :INTERFACE
           :fields fields}]))

(defn- create-type-system-enum [definition]
  (let [name (:name definition)
        enum-fields (:enum-fields definition)
        fields (create-type-system-fields enum-fields)]
    [name {:name name
           :kind :ENUM
           :fields (create-type-system-fields enum-fields)}]))

(defn- create-type-system-directive [definition]
  (let [name (:name definition)
        on (:directive-on-name definition)]
    [name {:name name
           :kind :DIRECTIVE
           :on on}]))

(defn- create-type-system-schema [definition]
  (let [schema-types (:schema-types definition)]
    (merge {:kind :SCHEMA}
           schema-types)))

(defn- create-type-system-definition [definition]
  (let [type (:type-system-type definition)]
    (log/debug "type: " type)
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
        (assoc-in [:fields "__schema"] {:name "__schema" :type-field-type {:name "__Schema"
                                                                           :required true}})
        (assoc-in [:fields "__type"] {:name "__type" :type-field-type {:name "__Type"}}))))

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
         root-query-type-name (or (get-in schema [:schema :query-type :name])
                                  "Query")
         ]
     {:schema schema
      :types (assoc-in (into default-types types)
                       [root-query-type-name :fields "__schema"]
                       {:name "__schema" :type-field-type {:name "__Schema"}})
      :interfaces (into {} interfaces)
      :unions (into {} unions)
      :inputs (into {} inputs)
      :enums (into {} enums)
      :directives (into {} directives)}))
  ([parsed-schema]
   (create-schema parsed-schema nil)))

(defn inject-introspection-schema [schema introspection-schema]
  "Combine schema definition with introspection schema"
  (-> schema
      (update :types (fn [types]
                       (-> (merge types (:types introspection-schema))
                           (update (get-in schema [:schema :query-type :name])
                                   inject-introspection-root-query-fields))))
      (update :interfaces (merge (:interfaces introspection-schema)))
      (update :unions (merge (:unions introspection-schema)))
      (update :inputs (merge (:inputs introspection-schema)))
      (update :enums (merge (:enums introspection-schema)))
      (update :directives (merge (:directives introspection-schema)))))

;; (def ^{:private true}
;;   introspection-schema
;;   (-> (io/resource "introspection.schema")
;;       (slurp)
;;       ;; (parser/parse)
;;       ;; (parser/transform)
;;       ;; (create-schema)
;;       ))

(defn get-type-in-schema [schema type-name]
    "Get type definition for given 'type-name' from provided 'schema'."
  (if (nil? type-name)
    (throw (ex-info "get-type-in-schema: type-name is NULL!" {:type-name type-name})))
  ;; (log/debug "get-type-in-schema: schema: " schema " type-name: " type-name)
  (get-in schema [:types type-name]))

(defn get-root-query-type
  "Get root query type name from schema definition."
  [schema]
  ;; (log/debug "schema: " schema)
  (let [root-query-type-name (get-in schema [:schema :query-type :name])]
    (if root-query-type-name
      (get-type-in-schema schema root-query-type-name)
      (throw (ex-info (format "get-root-query-type: schema: '%s' doesn't have root query type definition." schema) {})))))

(defn get-root-mutation-type
  "Get root mutation type name from schema definition."
  [schema]
  ;; (log/debug "schema: " schema)
  (let [root-mutation-type-name (get-in schema [:schema :mutation-type :name])]
    (if root-mutation-type-name
      (get-type-in-schema schema root-mutation-type-name)
      (throw (ex-info (format "get-root-mutation-type: schema: '%s' doesn't have root mutation type definition." schema) {})))))

(defn get-field-type
  "Get the type of a field definied in given 'type-name'."
  [schema type-name field-name]
  (if (nil? type-name)
    (throw (ex-info "get-field-type: type-name is NULL!" {:type-name type-name
                                                           :field-name field-name})))
  (if (nil? field-name)
    (throw (ex-info "get-field-type: field-name is NULL!" {:type-name type-name
                                                           :field-name field-name})))
  (let [type (get-type-in-schema schema type-name)
        field-type (get-in type [:fields field-name :type-field-type])
        field-type-kind (:kind field-type)]
    (if (nil? field-type)
      (throw (ex-info (format "get-field-type: type-name: %s, field-name: %s, field-type: %s." type-name field-name field-type)
                      {:type-name type-name
                       :field-name field-name})))
    (if field-type-kind
      field-type ; when field type is LIST or NON_NULL
      (get-type-in-schema schema (:name field-type)))))

(defn get-inner-type
  "Get inner type of 'field-type'"
  [schema field-type]
  (let [inner-type (:innerType field-type)
        inner-type-kind (:kind inner-type)]
    (if inner-type-kind
      inner-type
      (if-let [inner-type-name (get-in inner-type [:type-field-type :name])]
        (get-type-in-schema schema inner-type-name)
        (throw (ex-info (format "get-inner-type: failed getting inner type of field-type(%s)" field-type)
                        {:field-type field-type}))))))

(comment
  "query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      subscriptionType { name }
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
