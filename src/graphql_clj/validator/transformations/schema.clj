(ns graphql-clj.validator.transformations.schema
  (:require [graphql-clj.type :as type]
            [graphql-clj.introspection :as intro]))

(defn mapify-schema
  "Create schema definition from parsed & transformed type system definition."
  ([parsed-schema]
   (let [definitions (:type-system-definitions parsed-schema)
         grouped (into {} (group-by :node-type definitions))
         schemas (:schema-definition grouped)               ;; TODO why?
         schema (or (first schemas) {:node-type :schema-definition :query-type {:name "Query"}}) ;; TODO why?
         root-query-type-name (get-in schema [:query-type :name]) ;; TODO why?
         sub-grouped (->> (dissoc grouped :schema-definition)
                          (map (fn [[k v]] [k (->> (map #(vector (name (:type-name %)) (dissoc % :type)) v) (into {}))]))
                          (into {}))]
     (assert (< (count schemas) 2) "No more than one schema is allowed!")
     {:schema     schema
      :types      (-> (into type/default-types (:type-definition sub-grouped))
                      (update-in [root-query-type-name] intro/upsert-root-query root-query-type-name))
      ;; All types within a GraphQL schema must have unique names. No two provided types may have the same name. No provided type may have a name which conflicts with any built in types (including Scalar and Introspection types).
      ;; All directives within a GraphQL schema must have unique names. A directive and a type may share the same name, since there is no ambiguity between them.
      :interfaces (get sub-grouped :interface-definition {}) ;; TODO combine with types
      :unions     (get sub-grouped :union-definition {})     ;; TODO combine with types
      :inputs     (get sub-grouped :input-definition {})     ;; TODO combine with types
      :enums      (get sub-grouped :enum-definition {})      ;; TODO combine with types
      :directives (get sub-grouped :directive-definition {})})))
