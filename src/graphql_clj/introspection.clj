(ns graphql-clj.introspection
  (:require [graphql-clj.parser :as parser]
            [clojure.java.io :as io]))

(def introspection-schema-str (slurp (io/resource "introspection.schema")))

(def introspection-schema (parser/parse introspection-schema-str))

(def root-query-schema-fields
  [{:field-name "__schema" :type-name "__Schema" :node-type :field :required true}
   {:field-name "__type" :type-name "__Type" :node-type :field}])

(defn- default-root-query-node [root-query-name]
  {:node-type :type-definition
   :type-name root-query-name
   :section   :type-system-definitions
   :fields    root-query-schema-fields
   :kind      :OBJECT})

(defn upsert-root-query [root-query-node root-query-name]
  (if root-query-node
    (update root-query-node :fields into root-query-schema-fields)
    (default-root-query-node root-query-name)))
