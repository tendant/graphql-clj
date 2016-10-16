(ns graphql-clj.validator
  (:require [graphql-clj.validator.rules.default-values-of-correct-type]
            [graphql-clj.validator.rules.arguments-of-correct-type]
            [graphql-clj.validator.rules.fields-on-correct-type]
            [graphql-clj.validator.rules.known-argument-names]
            [graphql-clj.validator.rules.known-type-names]
            [graphql-clj.validator.rules.known-fragment-names]
            [graphql-clj.validator.rules.variables-are-input-types]
            [graphql-clj.validator.rules.no-undefined-variables]
            [graphql-clj.validator.rules.no-fragment-cycles]
            [graphql-clj.validator.rules.fragments-on-composite-types]
            [graphql-clj.validator.rules.unique-variable-names]
            [graphql-clj.validator.rules.unique-operation-names]
            [graphql-clj.validator.rules.unique-input-field-names]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]
            [instaparse.core :as insta]
            [graphql-clj.error :as ge]))

(def first-pass-rules [spec/add-spec spec/define-specs])

(def second-pass-rules
  (flatten [graphql-clj.validator.rules.known-type-names/rules
            graphql-clj.validator.rules.known-argument-names/rules
            graphql-clj.validator.rules.known-fragment-names/rules
            graphql-clj.validator.rules.no-undefined-variables/rules
            graphql-clj.validator.rules.unique-input-field-names/rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules
            graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.variables-are-input-types/rules
            graphql-clj.validator.rules.fields-on-correct-type/rules
            graphql-clj.validator.rules.no-fragment-cycles/rules
            graphql-clj.validator.rules.fragments-on-composite-types/rules
            graphql-clj.validator.rules.unique-variable-names/rules
            graphql-clj.validator.rules.unique-operation-names/rules]))

(defn- validate [visit-fn]
  (try (visit-fn)
       (catch Exception e {:errors [(.getMessage e)]})))

(defn- guard-parsed [doc-type doc]
  (when (insta/failure? doc)
    (ge/throw-error (format "Syntax error in %s document at index" doc-type (:index doc)) {:doc doc})))

(defn- validate-schema*
  "Do a 2 pass validation of a schema
   - First pass to add specs and validate that all types resolve.
   - Second pass to apply all the validator rules.
   There may be a clever way to avoid 2 passes...but for now it seems more interesting to be feature complete"
  [schema] ;; TODO inject introspection schema?
  (guard-parsed "schema" schema)
  (let [s (visitor/initial-state schema)
        {:keys [document state]} (visitor/visit-document schema s first-pass-rules)
        second-pass (visitor/visit-document document state second-pass-rules)]
    (assoc (:state second-pass) :schema (:document second-pass))))

(defn validate-statement*
  "Do a 2 pass validation of a statement"
  [document' schema]
  (guard-parsed "schema" schema)
  (guard-parsed "statement" document')
  (let [s (assoc schema :statement-hash (hash document'))
        {:keys [document state]} (visitor/visit-document document' s first-pass-rules)]
    (visitor/visit-document document state second-pass-rules)))

;; Public API

(defn validate-schema [schema]
  (validate #(validate-schema* schema)))

(defn validate-statement [document schema]
  (validate #(validate-statement* document schema)))
