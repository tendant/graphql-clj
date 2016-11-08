(ns graphql-clj.type-test
  (:use graphql-clj.type)
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [graphql-clj.validator :as validator]))

(def simple-user-schema
  "type User {
  name: String
  nickname: String
}

type QueryRoot {
  user: User
}

schema {
  query: QueryRoot
}")

;; TODO move to validator test
(deftest test-simple-schema
  (let [parsed-schema (validator/validate-schema (parser/parse simple-user-schema))]
    (is (not (nil? (:document parsed-schema))))))



