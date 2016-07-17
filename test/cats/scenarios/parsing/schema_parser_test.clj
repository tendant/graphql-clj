(ns cats.scenarios.parsing.schema-parser-test
  (:require [clojure.test :refer :all]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]))

(def schema-parser-tests (yaml/from-file "test/cats/scenarios/parsing/SchemaParser.yaml"))

(assert (not (nil? schema-parser-tests)) "No schema tests found!")

(deftest test-schema-parser-tests
  (testing "test SchemaParser.yml"
    (doseq [t (get schema-parser-tests "tests")]
      (let [query (get-in t ["given" "query"])]
        (is (not (nil? (parser/parse query))))))))
