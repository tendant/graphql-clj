(ns graphql-clj.parser-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [graphql-clj.parser :as parser]
            [yaml.core :as yaml]
            [graphql-clj.test-helpers :as th]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

(def test-statements (edn/read-string (slurp "test/scenarios/statements.edn")))

(deftest parse-statements
  (doseq [statement test-statements]
    (testing (str "Test all statements parsing, statement: " statement)
      (is (not (insta/failure? (parser/parse statement)))))))

(def test-schemas (conj (edn/read-string (slurp "test/scenarios/schemas.edn"))
                        (slurp "test/scenarios/cats/validation/validation.schema.graphql")))

(deftest parse-schemas
  (doseq [schema test-schemas]
    (testing (str "Test schema parsing and transforming. schema: " schema)
      (is (not (insta/failure? (parser/parse schema)))))))

(defn schema-parser-tests [filename]
  (->> (get (yaml/from-file filename) "tests")
       (map th/parse-test-case)))

(deftest parse-cats-yaml
  (testing "SchemaParser.yaml"
    (doseq [{:keys [expected result]} (schema-parser-tests "test/scenarios/cats/parsing/SchemaParser.yaml")]
      (is (= expected result))))
  (testing "DefaultValuesOfCorrectType.yaml"
    (doseq [{:keys [result]} (schema-parser-tests "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml")]
      (is (= :passes result)))))

;;;;; Test Helpers for visualizing the parsed tree ;;;;;

(defn render-parsed! [to-filename parsed-data]
  (pp/pprint parsed-data (clojure.java.io/writer (str "test/scenarios/parsed/" to-filename))))

(defn render-all-parsed! []
  (render-parsed! "statements.edn" (mapv parser/parse test-statements))
  (render-parsed! "schemas.edn" (mapv parser/parse test-schemas)))
