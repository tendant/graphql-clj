(ns graphql-clj.validation.operations-test
  (:use [clojure.test :refer [deftest is testing]]
        [graphql-clj.test-helpers :refer [load-tests-from-yaml
                                          process-test-case]]
        [graphql-clj.validation :refer [validate-document]])
  (:require [yaml.core :as yaml]))

(def operations-tests (load-tests-from-yaml "graphql_clj/validation/operations_test.yaml"))

(def scheam nil)

(deftest test-operations-tests
  (doseq [t operations-tests]
    (let [test-case (process-test-case t)
          expected (:expected test-case)
          result (:result test-case)]
      (println "validated:" (:validated test-case))
      (testing (format "test operations (%s), when (%s), %s." (:name t) (:when test-case) (:result test-case))
        (is (= expected result))))))

