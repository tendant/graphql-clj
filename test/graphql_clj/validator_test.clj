(ns graphql-clj.validator-test
  (:require [clojure.test :refer :all]
            [graphql-clj.validator :as validator]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [graphql-clj.parser-test :as pt]
            [graphql-clj.test-helpers :as th]))

(deftest validate-schemas
  (doseq [schema pt/test-schemas]
    (testing (str "Test schema validation. schema: " schema)
      (let [validated (validator/validate-schema (parser/parse schema))]
        (is (:schema validated))
        (is (nil? (:errors validated)))))))

(def schema
  (-> (parser/parse (slurp "test/scenarios/cats/validation/validation.schema.graphql"))
      validator/validate-schema))

(assert (not (nil? schema)) "No schema found!")

(defn validate-test-case [{:keys [parsed] :as test-case}]
  (let [validated (validator/validate-statement parsed schema)]
    (assoc test-case :validated validated
                     :result (if (-> validated :state :errors empty?) :passes :errors))))

(def cats
  (->> [(get (yaml/from-file "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/ArgumentsOfCorrectType.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/FieldsOnCorrectType.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/KnownArgumentNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/KnownTypeNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/KnownFragmentNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/VariablesAreInputTypes.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/NoUndefinedVariables.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/NoFragmentCycles.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/FragmentsOnCompositeTypes.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueVariableNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueOperationNames.yaml") "tests")]
       flatten
       (map th/parse-test-case)
       (map validate-test-case)))

(defn- match-error [expected validated]
  (= (map :error expected) (->> validated :state :errors (map :error))))

(defn- expect-valid [{:keys [result expected]}]
  (= expected result))

(deftest default-values-of-correct-type
  (testing "valid1"
    (is (expect-valid (nth cats 0))))
  (testing "valid2"
    (let [{:keys [result expected]} (nth cats 1)]
      (is (= expected result))))
  (testing "valid3"
    (let [{:keys [result expected]} (nth cats 2)]
      (is (= expected result))))
  (testing "default-for-required-field"
    (let [{:keys [validated expected]} (nth cats 3)]
      (is (match-error expected validated))))
  (testing "bad-value-for-default"
    (let [{:keys [validated expected]} (nth cats 4)]
      (is (match-error expected validated))))
  (testing "complex variables missing required field"
    (let [{:keys [validated expected]} (nth cats 5)]
      (is (match-error expected validated)))))

(deftest arguments-of-correct-type
  (testing "valid"
    (is (expect-valid (nth cats 6))))
  (testing "bad-value"
    (let [{:keys [validated expected]} (nth cats 7)]
      (is (match-error expected validated)))))

(deftest fields-on-correct-type
  (testing "valid"
    (is (expect-valid (nth cats 8))))
  (testing "missing-type"
    (let [{:keys [validated expected]} (nth cats 9)]
      (is (match-error expected validated))))
  (testing "missing-type-nested"
    (let [{:keys [validated expected]} (nth cats 10)]
      (is (match-error expected validated))))
  (testing "missing-type-double-nested"
    (let [{:keys [validated expected]} (nth cats 11)]
      (is (match-error expected validated)))))

(deftest known-argument-names
  (testing "valid"
    (is (expect-valid (nth cats 12))))
  (testing "unknown argument name"
    (let [{:keys [validated expected]} (nth cats 13)]
      (is (match-error expected validated)))))

(deftest known-type-names
  (testing "valid variable definition"
    (is (expect-valid (nth cats 14))))
  (testing "unknown type name on variable definition"
    (let [{:keys [validated expected]} (nth cats 15)]
      (is (match-error expected validated))))
  (testing "valid fragment condition"
    (is (expect-valid (nth cats 16))))
  (testing "unknown type name on fragment condition"
    (let [{:keys [validated expected]} (nth cats 17)]
      (is (match-error expected validated)))))

(deftest known-fragment-names
  (testing "known fragment name"
      (is (expect-valid (nth cats 18))))
  (testing "unknown fragment name"
    (let [{:keys [validated expected]} (nth cats 19)]
      (is (match-error expected validated)))))

(deftest variables-are-input-types
  (testing "input type variable"
    (is (expect-valid (nth cats 20))))
  (testing "interface type variable"
    (let [{:keys [validated expected]} (nth cats 21)]
      (is (match-error expected validated)))))

(deftest no-undefined-variables
  (testing "undefined variable"
    (let [{:keys [validated expected]} (nth cats 22)]
      (is (match-error expected validated)))))

(deftest no-fragment-cycles
  (testing "fragment-cycle"
    (let [{:keys [validated expected]} (nth cats 23)]
      (is (match-error expected validated))))
  (testing "cyclical data"
    (let [{:keys [validated expected]} (nth cats 24)]
      (is (match-error expected validated)))))

(deftest fragments-on-composite-types
  (testing "valid inline fragment"
    (is (expect-valid (nth cats 25))))
  (testing "invalid inline fragment"
    (let [{:keys [validated expected]} (nth cats 26)]
      (is (match-error expected validated))))
  (testing "invalid fragment-definition"
    (let [{:keys [validated expected]} (nth cats 27)]
      (is (match-error expected validated)))))

(deftest unique-variable-names
  (testing "duplicate variable name"
    (let [{:keys [validated expected]} (nth cats 28)]
      (is (match-error expected validated)))))

(deftest unique-operation-names
  (testing "duplicate operation name"
    (let [{:keys [validated expected]} (nth cats 29)]
      (is (match-error expected validated))))
  (testing "multiple valid operations with unique names"
    (is (expect-valid (nth cats 30)))))
