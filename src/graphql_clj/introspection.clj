(ns graphql-clj.introspection
  (:require [graphql-clj.parser :as parser]
            [clojure.java.io :as io]))

(def introspection-schema-str (slurp (io/resource "introspection.schema")))

(def introspection-schema (parser/parse introspection-schema-str))

(def introspection-query (slurp (io/resource "introspection.graphql")))

(def root-query-schema-fields
  [{:field-name "__schema" :type-name "__Schema" :node-type :type-field :required true}
   {:field-name "__type" :type-name "__Type" :node-type :type-field}])

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

(defn type-resolver [type]
  (when (not (contains? #{:LIST :NON_NULL} (:kind type)))
    (assert (:type-name type) (format "type name is null for type: %s." type)))
  {:kind (:kind type)
   :name (:type-name type)
   :description (:description type)

   ;; OBJECT and INTERFACE only
   :fields (when (contains? #{:OBJECT :INTERFACE} (:kind type))
             (:fields type)) ; defer resolving of fields

   ;; OBJECT only
   :interfaces [] ; TODO

   ;; INTERFACE and UNION only
   :possibleTypes nil

   ;; ENUM only
   :enumValues (when (= :ENUM (:kind type))
                 (assert (:fields type) (format "enum values is empty for type: %s." type))
                 (:fields type))

   ;; INPUT_OBJECT only
   :inputFields [] ; TODO

   ;; NON_NULL and LIST only
   ;; :ofType nil
   :inner-type (:inner-type type) ; provide inner-type for ofType resolver
   })

(defn schema-types [schema]
  (->> (concat (vals (:types schema))
               (vals (:interfaces schema))
               (vals (:enums schema)))
      (map type-resolver)))

(defn field-resolver [field]
  (assert (:field-name field) (format "field name is null for field: %s." field))
  {:name (:field-name field)
   :description (:description field)
   :args [] ; TODO
   
   ;; :type nil ; defer resolving of type
   :type-name (:type-name field)
   :kind (:kind field)
   :inner-type (:inner-type field)
   
   :isDeprecated false ; TODO
   :deprecationReason nil ; TODO
   })

(defn enum-resolver [enum]
  {:name (:name enum)
   :description nil
   :isDeprecated nil
   :deprecationReason nil})
