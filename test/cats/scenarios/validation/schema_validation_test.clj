(ns cats.scenarios.validation.schema-validation-test
  (:require [clojure.test :refer :all]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [cats.scenarios.test-helpers :as th]))

(def schema-validation-tests
  (->> [(get (yaml/from-file "test/cats/scenarios/validation/DefaultValuesOfCorrectType.yaml") "tests")]
       flatten
       (map th/parse-test-case)))

(assert (not (nil? schema-validation-tests)) "No schema validation tests found!")

(def schema
  (parser/parse (slurp "test/cats/scenarios/validation/validation.schema.graphql")))

(deftest parsing-validation-yaml
  (testing "DefaultValuesOfCorrectType.yaml"
    (doseq [{:keys [result]} schema-validation-tests]
      (is (= :passes result)))))
