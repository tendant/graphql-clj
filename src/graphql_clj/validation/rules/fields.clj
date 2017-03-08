(ns graphql-clj.validation.rules.fields
  (:require [zip.visit :as zv]
            [clojure.string :as s]
            [graphql-clj.type :as type]))

;;; 5.2 Fields

;; 5.2.1 Field Selections on Objects, Interfaces, and Unions Types
;;
;; Formal Specification
;; 
;; * For each selection in the document.
;; * Let fieldName be the target field of selection
;; * fieldName must be defined on type in scope

(defn validate-field-name-in-type
  [type field-name]
  (let [fields (map :graphql-clj/field-name (:graphql-clj/type-fields type))]
    (if (and (not (contains? fields field-name))
             (s/starts-with? field-name "__"))
      (format "Type(%s) does not contain field(%s)." type field-name))))

(defn validate-inline-fragment-type-name
  [type type-name]
  (let [type-names (set (:graphql-clj/type-names type))]
    (if (not (contains? type-names type-name))
      (format "Type(%s) does not contain type(%s)." type type-name))))

(defn validate-field-in-type
  [schema type selection]
  (case (:graphql-clj/node-type selection)
    :graphql-clj/field (validate-field-name-in-type type (:graphql-clj/field-name selection))
    :graphql-clj/inline-fragment (validate-inline-fragment-type-name type (get-in selection [:graphql-clj/type-condition :graphql-clj/type-name]))))

(defn validate-fields-in-type
  "Validate fields in type, return nil if there is no error, return list of error messages for all validation errors."
  [schema type-name selection-set]
  (let [type (get-in schema [:types type-name])]
    (-> selection-set
        (map #(validate-field-in-type schema type %))
        (remove nil?))))

(zv/defvisitor field-name-must-be-defined :pre [n s]
  (when-let [selection-set (:graphql-clj/selection-set n)]
    (println "selection-set for field rules:" selection-set)
    (println "node: " n)
    (let [fields-selection (->> selection-set
                                (filter #(= :graphql-clj/field (:graphql-clj/node-type %)))
                                (filter #(not (s/starts-with? (:graphql-clj/field-name %) "__"))))
          _ (println "fields-selection:" fields-selection)
          selection-field-names (set (map :graphql-clj/field-name fields-selection))
          node-type (:graphql-clj/node-type n)
          type-name (or (get-in n [:graphql-clj/type-condition :graphql-clj/type-name])
                        (when (:graphql-clj/operation-type n)
                          (get-in schema [:graphql/schema :graphql-clj/query-type :graphql-clj/name])) ; FIXME: also handle mutation type
                        )
          schema (:schema s)
          type (type/get-type-in-schema schema type-name)
          type-fields (:graphql-clj/type-fields type)
          type-field-names (set (map :graphql-clj/field-name type-fields))]
      (assert type (format "No type is defined for type-name:%s." type-name))
      (let [errors (validate-fields-in-type schema type-name selection-set)]
        (if (seq errors)
          {:node n
           :state (update s :errors #(concat % errors))}
          {:node n
           :state s})))))



(def rules [field-name-must-be-defined])
