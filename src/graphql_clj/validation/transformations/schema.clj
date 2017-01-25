(ns graphql-clj.validation.transformations.schema
  (:require [graphql-clj.type :as type]))

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
      :graphql-clj/types      (-> (into type/default-types (:graphql-clj/type-definition sub-grouped))
                      (into (get sub-grouped :graphql-clj/interface-definition))
                      (into (get sub-grouped :graphql-clj/union-definition))
                      (into (get sub-grouped :graphql-clj/input-definition))
                      (into (get sub-grouped :graphql-clj/enum-definition)))
      :graphql-clj/directives (get sub-grouped :graphql-clj/directive-definition {})})))


