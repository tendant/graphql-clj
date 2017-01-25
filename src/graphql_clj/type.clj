(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]
            [graphql-clj.validator :as validator]
            [graphql-clj.introspection :as intro]
            [graphql-clj.validation.transformations.schema :as ts]))

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

(defn ^:deprecated create-schema
  ([parsed-schema] (create-schema parsed-schema intro/introspection-schema))
  ([parsed-schema _introspection-schema]
   (let [schema-with-intro (inject-introspection-schema parsed-schema)]
     (ts/mapify-schema schema-with-intro))))
