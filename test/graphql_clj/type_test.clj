(ns graphql-clj.type-test
  (:use graphql-clj.type)
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]))

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

(deftest test-simple-schema
  (let [parsed-schema (parser/parse simple-user-schema)]
    (is (not (nil? (create-schema parsed-schema))))))



