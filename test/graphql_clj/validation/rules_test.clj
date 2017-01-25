(ns graphql-clj.validation.rules-test
  (:use [clojure.test :refer [deftest is testing]]
        [graphql-clj.test-helpers :refer [load-tests-from-yaml
                                          process-test-case]]
        [graphql-clj.validation :refer [validate-document]])
  (:require [yaml.core :as yaml]
            [clojure.java.io :as io]
            [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]))

(def operations-tests (load-tests-from-yaml "graphql_clj/validation/operations_test.yaml"))
(def fields-tests (load-tests-from-yaml "graphql_clj/validation/fields_test.yaml"))

(deftest test-operations-tests
  (doseq [t operations-tests]
    (let [test-case (process-test-case t)
          expected (:expected test-case)
          result (:result test-case)]
      (testing (format "test operations: (%s), when (%s), %s." (:name t) (:when test-case) (:result test-case))
        (is (= expected result))))))

(deftest test-fields-tests
  (doseq [t fields-tests]
    (let [test-case (process-test-case t)
          expected (:expected test-case)
          result (:result test-case)]
      (testing (format "test fields: (%s), when (%s), %s." (:name t) (:when test-case) (:result test-case))
        (is (= expected result))))))

