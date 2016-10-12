(ns graphql-clj.validator-test
  (:require [clojure.test :refer :all]
            [graphql-clj.validator :as validator]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [graphql-clj.parser-test :as pt]
            [graphql-clj.test-helpers :as th]
            [clojure.spec :as s]))

(deftest validate-schemas
  (doseq [schema pt/test-schemas]
    (testing (str "Test schema validation. schema: " schema)
      (let [validated (validator/validate-schema (parser/parse schema))]
        (is validated)))))

(def schema
  (-> (parser/parse (slurp "test/scenarios/cats/validation/validation.schema.graphql"))
      validator/validate-schema))

(assert (not (nil? schema)) "No schema found!")

(defn- operation-errors [validated]
  (->> validated :state :operation-definitions :errors))

(defn validate-test-case [{:keys [parsed] :as test-case}]
  (let [validated (validator/validate-statement parsed schema)]
    (assoc test-case :validated validated
                     :result (if (operation-errors validated) :errors :passes))))

(def cats
  (->> [(get (yaml/from-file "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/ArgumentsOfCorrectType.yaml") "tests")]
       flatten
       (map th/parse-test-case)
       (map validate-test-case)))

(defn- match-error [expected validated]
  (= (map :error expected) (->> validated operation-errors (map :error))))

(deftest default-values-of-correct-type
  (testing "valid"
    (let [{:keys [result expected]} (nth cats 2)]
      (is (= expected result))))
  (testing "default-for-required-field"
    (let [{:keys [validated expected]} (nth cats 3)]
      (is (match-error expected validated))))
  (testing "bad-value-for-default"
    (let [{:keys [validated expected]} (nth cats 4)]
      (is (match-error expected validated))))
  (testing "complex variables missing required field"       ;; TODO variables can be the same across different queries, need to disambiguate them.  TODO why does $d not work here?
    (let [{:keys [validated expected]} (nth cats 5)]
      (is (match-error expected validated)))))

(deftest arguments-of-correct-type                        ;; crazy not having the schema hash
  (testing "bad-value"
    (let [{:keys [validated expected]} (nth cats 6)]
      (is (match-error expected validated)))))
