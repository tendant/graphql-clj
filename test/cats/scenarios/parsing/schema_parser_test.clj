(ns cats.scenarios.parsing.schema-parser-test
  (:require [clojure.test :refer :all]
            [yaml.core :as yaml]
            [cats.scenarios.test-helpers :as th]))

(def schema-parser-tests (get (yaml/from-file "test/cats/scenarios/parsing/SchemaParser.yaml") "tests"))

(assert (not (nil? schema-parser-tests)) "No schema parsing tests found!")

(def parser-results (map th/parse-test-case schema-parser-tests))

(deftest schema-parser
  (testing "SchemaParser.yaml"
    (doseq [{:keys [expected result]} parser-results]
      (is (= expected result)))))
