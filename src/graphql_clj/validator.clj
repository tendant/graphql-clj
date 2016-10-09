(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]
            [graphql-clj.validator.rules.arguments-of-correct-type]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]))

(def specified-rules
  (flatten [[spec/add-spec]
            graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules]))

;; TODO visit and validate schema and queries using memoization

(defn validate
  ([schema]
   (visitor/visit-document schema [spec/add-spec]))
  ([_schema document]
    ;; TODO no use for schema?
   (visitor/visit-document document specified-rules)))       ;; TODO collect state and nodes afterwards
