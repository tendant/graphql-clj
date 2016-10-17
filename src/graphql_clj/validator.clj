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
            [graphql-clj.validator.rules.unique-fragment-names]
            [graphql-clj.validator.rules.unique-argument-names]
            [graphql-clj.validator.rules.provided-non-null-arguments]
            [graphql-clj.validator.rules.no-unused-variables]
            [graphql-clj.validator.rules.no-unused-fragments]
            [graphql-clj.validator.rules.known-directives]
            [graphql-clj.validator.rules.lone-anonymous-operation]
            [graphql-clj.validator.rules.variables-in-allowed-position]
            [graphql-clj.visitor :as visitor]
            [graphql-clj.spec :as spec]
            [instaparse.core :as insta]
            [graphql-clj.error :as ge]))

(def first-pass-rules [spec/add-spec spec/define-specs])

(def second-pass-rules-schema
  (flatten [graphql-clj.validator.rules.unique-input-field-names/schema-rules
            graphql-clj.validator.rules.unique-argument-names/schema-rules]))

(def second-pass-rules-statement
  (flatten [graphql-clj.validator.rules.lone-anonymous-operation/rules
            graphql-clj.validator.rules.known-type-names/rules
            graphql-clj.validator.rules.known-argument-names/rules
            graphql-clj.validator.rules.known-fragment-names/rules
            graphql-clj.validator.rules.no-undefined-variables/rules
            graphql-clj.validator.rules.unique-input-field-names/statement-rules
            graphql-clj.validator.rules.arguments-of-correct-type/rules
            graphql-clj.validator.rules.default-values-of-correct-type/rules
            graphql-clj.validator.rules.variables-are-input-types/rules
            graphql-clj.validator.rules.fields-on-correct-type/rules
            graphql-clj.validator.rules.no-fragment-cycles/rules
            graphql-clj.validator.rules.fragments-on-composite-types/rules
            graphql-clj.validator.rules.unique-variable-names/rules
            graphql-clj.validator.rules.unique-operation-names/rules
            graphql-clj.validator.rules.unique-fragment-names/rules
            graphql-clj.validator.rules.unique-argument-names/statement-rules
            graphql-clj.validator.rules.provided-non-null-arguments/rules
            graphql-clj.validator.rules.no-unused-variables/rules
            graphql-clj.validator.rules.no-unused-fragments/rules
            graphql-clj.validator.rules.known-directives/rules
            graphql-clj.validator.rules.variables-in-allowed-position/rules]))

(defn- validate [visit-fn]
  (try (visit-fn)
       (catch Exception e {:errors [(.getMessage e)]})))

(defn- guard-parsed [doc-type doc]
  (when (insta/failure? doc)
    (ge/throw-error (format "Syntax error in %s document at index" doc-type (:index doc)) {:doc doc})))

(defn- validate-schema*
  "Do a 2 pass validation of a schema
   - First pass to add specs and validate that all types resolve.
   - Second pass to apply all the validator rules."
  [schema rules1 rules2] ;; TODO inject introspection schema?
  (guard-parsed "schema" schema)
  (let [s (visitor/initial-state schema)
        {:keys [document state]} (visitor/visit-document schema s rules1)
        second-pass (visitor/visit-document document state rules2)]
    (assoc-in second-pass [:state :schema] (:document second-pass))))

(defn validate-statement*
  "Do a 2 pass validation of a statement"
  [document' state rules1 rules2]
  (guard-parsed "schema" state)
  (guard-parsed "statement" document')
  (let [s (assoc state :statement-hash (hash document'))
        {:keys [document state]} (visitor/visit-document document' s rules1)]
    (visitor/visit-document document state rules2)))

;; Public API

(defn validate-schema
  ([schema]
   (validate-schema schema second-pass-rules-schema))
  ([schema rules2]
   (validate #(validate-schema* schema first-pass-rules rules2))))

(defn validate-statement
  ([document state]
   (validate-statement document state second-pass-rules-statement))
  ([document state rules2]
   (validate #(validate-statement* document state first-pass-rules rules2))))
