(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]
            [graphql-clj.validator.rules.arguments-of-correct-type]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]
            [clojure.spec :as s]))

(def first-pass-rules
  [spec/keywordize spec/add-spec spec/define-specs])

(def second-pass-rules
  (flatten [graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules]))

(def specified-rules (into first-pass-rules second-pass-rules))

(defn- validate [visit-fn ]
  (try (visit-fn)
       (catch Exception e {:errors [(.getMessage e)]})))

(defn- validate-schema*
  "Do a 2 pass validation
   - First pass to add specs and validate that all types resolve.
   - Second pass to apply all the validator rules.
   There may be a clever way to avoid 2 passes...but for now it seems more interesting to be feature complete"
  [schema] ;; TODO inject introspection schema?
  (let [s (visitor/initial-state schema)
        {:keys [document]} (visitor/visit-document schema s first-pass-rules)]
    (assoc s :schema (:document (visitor/visit-document document s second-pass-rules)))))

;; Public API

(defn validate-schema [schema]
  (validate #(validate-schema* schema)))

(defn validate-statement [document schema]
  (validate #(visitor/visit-document document schema specified-rules)))
