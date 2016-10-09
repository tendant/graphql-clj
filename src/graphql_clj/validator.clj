(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]
            [graphql-clj.validator.rules.arguments-of-correct-type]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]))

(def specified-rules
  (flatten [graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules]))

(defn validate
  ([schema]
   (visitor/visit-document schema [spec/add-spec]))
  ([schema document]
   (let [document' (:document (visitor/visit-document document (visitor/query-root-mapping (:document schema)) [spec/add-spec]))]
     (visitor/visit-document document' specified-rules))))
