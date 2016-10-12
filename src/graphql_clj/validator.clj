(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]
            [graphql-clj.validator.rules.arguments-of-correct-type]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]))

(def specified-rules
  (flatten [[spec/keywordize spec/add-spec]
            graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules]))

(defn validate-schema [schema]                              ;; TODO inject introspection schema?
  (let [s (visitor/initial-state schema)
        validated-schema (visitor/visit-document schema s specified-rules)]
    (assoc s :schema (:document validated-schema))))

(defn validate-statement [document schema]
  (visitor/visit-document document schema specified-rules))
