(ns graphql-clj.validator.transformations.schema
  (:require [graphql-clj.introspection :as intro]
            [graphql-clj.spec :as spec]))

(defn mapify-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema]
   (let [definitions (:type-system-definitions parsed-schema)
         grouped (into {} (group-by :node-type definitions))
         schemas (:schema-definition grouped)
         schema (or (first schemas) {:node-type :schema-definition :query-type {:name "Query"}})
         root-query-type-name (get-in schema [:query-type :name])
         sub-grouped (->> (dissoc grouped :schema-definition)
                          (map (fn [[k v]] [k (->> (map #(vector (name (:type-name %)) (dissoc % :type)) v) (into {}))]))
                          (into {}))]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:schema     schema
      ;; All types within a GraphQL schema must have unique names. No two provided types may have the same name. No provided type may have a name which conflicts with any built in types (including Scalar and Introspection types).
      ;; All directives within a GraphQL schema must have unique names. A directive and a type may share the same name, since there is no ambiguity between them.
      :types      (-> (into spec/default-types (:type-definition sub-grouped))
                      (update-in [root-query-type-name] intro/upsert-root-query root-query-type-name)
                      (into (get sub-grouped :interface-definition))
                      (into (get sub-grouped :union-definition))
                      (into (get sub-grouped :input-definition))
                      (into (get sub-grouped :enum-definition)))
      :directives (get sub-grouped :directive-definition {})})))
