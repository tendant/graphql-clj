(ns graphql-clj.resolver-test
  (:require [clojure.test :refer :all]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.schema-validator :as sv]
            [graphql-clj.introspection :as intro]))

(def test-schema-str "type QueryRoot {
  id: String
  }")

(def test-schema (sv/validate-schema test-schema-str))

(deftest test-default-resolver
  (testing "default resolver with value"
    (let [type-name "NoNameType"
          field-name "testfield"
          test-field-value "testvalue"
          resolver (resolver/default-resolver-fn type-name field-name)
          result (resolver nil {:testfield test-field-value} nil)]
      (is (= test-field-value result)))))

(deftest test-schema-introspection-resolver
  (let [schema-resolver (resolver/schema-introspection-resolver-fn test-schema)]
    (testing "testing __schema"
      (is (not (nil? (schema-resolver "QueryRoot" "__schema")))))))

(deftest test-create-resolver-fn
  (testing "test create resolver fn"
    (let [resolver-fn (resolver/create-resolver-fn test-schema nil)]
      (testing "testing __schema"
        (is (not (nil? (resolver-fn "Query" "__schema")))))
      (testing "testing default field resolver"
        (let [resolver (resolver-fn "User" "fullname")]
          (is (= "Test User Fullname"
                 (resolver nil {:fullname "Test User Fullname"} nil))))))))
