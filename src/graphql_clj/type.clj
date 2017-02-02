(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]
            [graphql-clj.validator :as validator]
            [graphql-clj.introspection :as intro]))

(def default-types
  {"Int"     {:type-name "Int"     :kind :SCALAR}
   "Float"   {:type-name "Float"   :kind :SCALAR}
   "String"  {:type-name "String"  :kind :SCALAR}
   "Boolean" {:type-name "Boolean" :kind :SCALAR}
   "ID"      {:type-name "ID"      :kind :SCALAR}})

(defn get-type-in-schema
  "Get type definition for given 'type-name' from provided 'schema'."
  [schema type-name]
  (when (nil? type-name) (gerror/throw-error "get-type-in-schema: type-name is NULL!"))
  (get-in schema [:graphql-clj/types type-name]))

(defn- inject-introspection-schema
  "Given a schema definition, add internal introspection type system definitions,
   unless we are processing the introspection schema itself."
  [schema]
  (if (= schema intro/introspection-schema)
    schema
    (update schema :graphql-clj/type-system-definitions
            concat (:graphql-clj/type-system-definitions intro/introspection-schema))))

(defn mapify-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema]
   (let [definitions (:graphql-clj/type-system-definitions parsed-schema)
         grouped (into {} (group-by :graphql-clj/node-type definitions))
         schemas (:graphql-clj/schema-definition grouped)
         schema (or (first schemas) {:graphql-clj/node-type :graphql-clj/schema-definition :graphql-clj/query-type {:graphql-clj/name "Query"}})
         sub-grouped (->> (dissoc grouped :graphql-clj/schema-definition)
                          (map (fn [[k v]] [k (->> (map #(vector (name (:graphql-clj/type-name %)) (dissoc % :graphql-clj/type)) v) (into {}))]))
                          (into {}))]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:graphql-clj/schema     schema
      ;; All types within a GraphQL schema must have unique names. No two provided types may have the same name. No provided type may have a name which conflicts with any built in types (including Scalar and Introspection types).
      ;; All directives within a GraphQL schema must have unique names. A directive and a type may share the same name, since there is no ambiguity between them.
      :graphql-clj/types      (-> (into default-types (:graphql-clj/type-definition sub-grouped))
                      (into (get sub-grouped :graphql-clj/interface-definition))
                      (into (get sub-grouped :graphql-clj/union-definition))
                      (into (get sub-grouped :graphql-clj/input-definition))
                      (into (get sub-grouped :graphql-clj/enum-definition)))
      :graphql-clj/directives (get sub-grouped :graphql-clj/directive-definition {})})))

(defn ^:deprecated create-schema
  ([parsed-schema] (create-schema parsed-schema intro/introspection-schema))
  ([parsed-schema _introspection-schema]
   (let [schema-with-intro (inject-introspection-schema parsed-schema)]
     (mapify-schema schema-with-intro))))
