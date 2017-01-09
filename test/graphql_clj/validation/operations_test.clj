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
    (testing (str "test operations:" (:name t))
      (println "t:" t)
      (let [test-case (process-test-case t)
            parsed (:parsed test-case)
            parsed-result (:parsed-result test-case)
            validated-result (:validated-result test-case)
            expected (:expected test-case)
            result (case (:when test-case)
                     :parse parsed-result
                     :validate validated-result)]
        (is (= expected result))))))

