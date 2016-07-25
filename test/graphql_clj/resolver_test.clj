(ns graphql-clj.resolver-test
  (:use graphql-clj.resolver)
  (:require [clojure.test :refer :all]))

(deftest test-default-resolver
  (testing "default resolver with value"
    (let [type-name nil
          field-name "testfield"
          test-field-value "testvalue"
          resolver (default-resolver-fn type-name field-name)
          result (resolver nil {:testfield test-field-value})]
      (is (= test-field-value result)))))

(deftest test-schema-introspection-resolver
  (let [schema-resolver (schema-introspection-resolver-fn nil)]
    (testing "testing __schema"
      (is (not (nil? (schema-resolver "QueryRoot" "__schema")))))))

(deftest test-create-resolver-fn
  (testing "test create resolver fn"
    (let [resolver-fn (create-resolver-fn nil nil)]
      (testing "testing __schema"
        (is (not (nil? (resolver-fn "QueryRoot" "__schema")))))
      (testing "testing default field resolver"
        (let [resolver (resolver-fn "User" "fullname")]
          (is (= "Test User Fullname"
                 (resolver nil {:fullname "Test User Fullname"}))))))))
