(ns graphql-clj.resolver-test
  (:use graphql-clj.resolver)
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [graphql-clj.introspection :as introspection]))

(def test-schema (type/create-schema nil (parser/parse introspection/introspection-schema)))

(deftest test-default-resolver
  (testing "default resolver with value"
    (let [type-name "NoNameType"
          field-name "testfield"
          test-field-value "testvalue"
          resolver (default-resolver-fn type-name field-name)
          result (resolver nil {:testfield test-field-value})]
      (is (= test-field-value result)))))

(deftest test-schema-introspection-resolver
  (let [schema-resolver (schema-introspection-resolver-fn test-schema)]
    (testing "testing __schema"
      (is (not (nil? (schema-resolver "Query" "__schema")))))))

(deftest test-create-resolver-fn
  (testing "test create resolver fn"
    (let [resolver-fn (create-resolver-fn test-schema nil)]
      (testing "testing __schema"
        (is (not (nil? (resolver-fn "Query" "__schema")))))
      (testing "testing default field resolver"
        (let [resolver (resolver-fn "User" "fullname")]
          (is (= "Test User Fullname"
                 (resolver nil {:fullname "Test User Fullname"}))))))))
