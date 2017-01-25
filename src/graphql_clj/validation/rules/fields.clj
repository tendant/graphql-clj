(ns graphql-clj.validation.rules.fields
  (:require [zip.visit :as zv]
            [graphql-clj.type :as type]))

;;; 5.2 Fields

;; 5.2.1 Field Selections on Objects, Interfaces, and Unions Types
;;
;; Formal Specification
;; 
;; * For each selection in the document.
;; * Let fieldName be the target field of selection
;; * fieldName must be defined on type in scope

(zv/defvisitor field-name-must-be-defined :pre [n s]
  (if-let [selection-set (:graphql-clj/selection-set n)]
    (let [selection-field-names (set (map :graphql-clj/field-name selection-set))
          node-type (:graphql-clj/node-type n)
          type-name (get-in n [:graphql-clj/type-condition :graphql-clj/type-name])
          schema (:schema s)
          type (type/get-type-in-schema schema type-name)
          type-fields (:graphql-clj/type-fields type)
          type-field-names (set (map :graphql-clj/field-name type-fields))]
      (assert type (format "No type is defined for type-name:%s!" type-name))
      (println "fields: schema:" schema)
      (println "fields: node-type:" node-type)
      (println "fields: type-name:" type-name)
      (println "fields: type:" type)
      (println "fields: selection-field-names:" selection-field-names)
      (println "fields: type-field-names:" type-field-names)
      (let [diff (clojure.set/difference selection-field-names type-field-names)]
        (if (seq diff)
          {:node n
           :state {:errors (concat (:errors s)
                                   [(format "ERROR: field selection(%s) is undefined in type fields." diff)])}}
          {:node n
           :state s})))))

(def rules [field-name-must-be-defined])
