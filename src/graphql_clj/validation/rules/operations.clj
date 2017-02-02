(ns graphql-clj.validation.rules.operations
  (:require [zip.visit :as zv]))

;;; 5.1 Operations

;;; 5.1.1 Named Operation Definitions

;; 5.1.1.1 Operation Name Uniqueness
;; 
;; Formal Specification
;; For each operation definition operation in the document
;; Let operationName be the name of operation.
;; If operationName exists
;; Let operations be all operation definitions in the document named operationName.
;; operations must be a set of one.

(zv/defvisitor operation-definitions-visitor :pre [n s]
  (if-let [definitions (:graphql-clj/operation-definitions n)]
    (let [duplicates  (->> (map :graphql-clj/name definitions)
                           frequencies
                           (filter #(> (second %) 1))
                           (map (fn mfn [[n c]] (format "Operation name (%s) has been used more than once(%d). " n c))))]
      (if (seq duplicates)
        {:node n
         :state (update s :errors #(concat % duplicates))}
        {:node n
         :state s}))))

;;; 5.1.2 Anonymous Operation Definitions

;; 5.1.2.1 Lone Anonymous Operation
;;
;; Formal Specification
;;
;; 1. Let operations be all operation definitions in the document.
;; 2. Let anonymous be all anonymous operation definitions in the document.
;; 3. If operations is a set of more than 1:
;;   - anonymous must be empty.

(zv/defvisitor lone-anonymous-operation-visitor :pre [n s]
  (if-let [definitions (:graphql-clj/operation-definitions n)]
    (let [anonymous (remove :graphql-clj/name definitions)]
      (if (and (seq anonymous)
               (> (count definitions) 1))
        {:node n
         :state (update s :errors #(concat % [(format "Only one anonymouse operation is allowed!")]))}
        {:node n
         :state s}))))

(def rules [operation-definitions-visitor lone-anonymous-operation-visitor])
