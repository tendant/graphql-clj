(ns graphql-clj.type
  (:require [graphql-clj.error :as gerror]
            [graphql-clj.validator :as validator]))

(defn get-type-in-schema
  "Get type definition for given 'type-name' from provided 'schema'."
  [schema type-name]
  (when (nil? type-name) (gerror/throw-error "get-type-in-schema: type-name is NULL!"))
  (get-in schema [:types type-name]))

(defn ^:deprecated create-schema
  ([parsed-schema] (validator/validate-schema parsed-schema))
  ([parsed-schema _introspection-schema] (create-schema parsed-schema)))
