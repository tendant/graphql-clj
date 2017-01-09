(ns graphql-clj.validation.rules.fields
  (:require [zip.visit :as zv]))

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
    (let [node-type (:graphql-clj/node-type n)]
      )))

