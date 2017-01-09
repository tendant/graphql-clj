(ns graphql-clj.validation-test
  (:use graphql-clj.validation
        clojure.test
        graphql-clj.test-helpers)
  (:require [graphql-clj.fixture :as fixture]
            [graphql-clj.parser :as parser]))

(def schema (-> fixture/validation-5-schema-str
                parser/parse
                validate-schema))

;;; 5.1 Operations

;;; 5.1.1 Named Operation Definitions
;;; 5.1.1.1 Operation Name Uniqueness

;; Formal Specification

;; 1. For each operation definition operation in the document
;; 2. Let operationName be the name of operation.
;; 3. If operationName exists
;;      - Let operations be all operation definitions in the document named operationName.
;;      - operations must be a set of one.

(def operation-name-uniqueness-5-1-1-1-valid-query-str
  "query getDogName {
  dog {
    name
  }
}

query getOwnerName {
  dog {
    owner {
      name
    }
  }
}")

(def operation-name-uniqueness-5-1-1-1-invalid-query-str
  "query getName {
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}")

(def operation-name-uniqueness-5-1-1-1-invalid-2-query-str
  "query dogOperation {
  dog {
    name
  }
}

mutation dogOperation {
  mutateDog {
    id
  }
}")

(deftest operations-5.1.1.1
  (is (empty? (get-in (-> operation-name-uniqueness-5-1-1-1-valid-query-str
                           parser/parse
                           validate-document)
                      [:state :errors])))
  (is (get-in (-> operation-name-uniqueness-5-1-1-1-invalid-query-str
                  parser/parse
                  validate-document)
              [:state :errors]))
  (is (get-in (-> operation-name-uniqueness-5-1-1-1-invalid-2-query-str
                  parser/parse
                  validate-document)
              [:state :errors])))



;;; 5.1.2 Anonymous Operation Definitions
;;; 5.1.2.1 Lone Anonymous Operation
;; Formal Specification
;; 1. Let operations be all operation definitions in the document.
;; 2. Let anonymous be all anonymous operation definitions in the document.
;; 3. If operations is a set of more than 1:
;;     - anonymous must be empty.

(def lone-anonymous-operation-5-1-2-1-valid-query-str
  "")

