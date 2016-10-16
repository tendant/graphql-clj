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
      (let [validated (:state (validator/validate-schema (parser/parse schema)))]
        (is (:schema validated))
        (is (nil? (:errors validated)))))))

(def schema
  (-> (parser/parse (slurp "test/scenarios/cats/validation/validation.schema.graphql"))
      validator/validate-schema
      :state))

(assert (not (nil? schema)) "No schema found!")

(defn validate-test-case [{:keys [type parsed] :as test-case}]
  (let [validated (if (= :schema type)
                    (validator/validate-schema parsed)
                    (validator/validate-statement parsed schema))]
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
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueOperationNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueInputFieldNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueFragmentNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/UniqueArgumentNames.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/ProvidedNonNullArguments.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/NoUnusedVariables.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/NoUnusedFragments.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/KnownDirectives.yaml") "tests")]
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
  (testing "unknown argument name on field"
    (let [{:keys [validated expected]} (nth cats 13)]
      (is (match-error expected validated))))
  (testing "unknown argument name on directive"
    (let [{:keys [validated expected]} (nth cats 14)]
      (is (match-error expected validated)))))

(deftest known-type-names
  (testing "valid variable definition"
    (is (expect-valid (nth cats 15))))
  (testing "unknown type name on variable definition"
    (let [{:keys [validated expected]} (nth cats 16)]
      (is (match-error expected validated))))
  (testing "valid fragment condition"
    (is (expect-valid (nth cats 17))))
  (testing "unknown type name on fragment condition"
    (let [{:keys [validated expected]} (nth cats 18)]
      (is (match-error expected validated)))))

(deftest known-fragment-names
  (testing "known fragment name"
      (is (expect-valid (nth cats 19))))
  (testing "unknown fragment name"
    (let [{:keys [validated expected]} (nth cats 20)]
      (is (match-error expected validated)))))

(deftest variables-are-input-types
  (testing "input type variable"
    (is (expect-valid (nth cats 21))))
  (testing "interface type variable"
    (let [{:keys [validated expected]} (nth cats 22)]
      (is (match-error expected validated)))))

(deftest no-undefined-variables
  (testing "undefined variable"
    (let [{:keys [validated expected]} (nth cats 23)]
      (is (match-error expected validated)))))

(deftest no-fragment-cycles
  (testing "fragment-cycle"
    (let [{:keys [validated expected]} (nth cats 24)]
      (is (match-error expected validated))))
  (testing "cyclical data"
    (let [{:keys [validated expected]} (nth cats 25)]
      (is (match-error expected validated)))))

(deftest fragments-on-composite-types
  (testing "valid inline fragment"
    (is (expect-valid (nth cats 26))))
  (testing "invalid inline fragment"
    (let [{:keys [validated expected]} (nth cats 27)]
      (is (match-error expected validated))))
  (testing "invalid fragment-definition"
    (let [{:keys [validated expected]} (nth cats 28)]
      (is (match-error expected validated)))))

(deftest unique-variable-names
  (testing "duplicate variable name"
    (let [{:keys [validated expected]} (nth cats 29)]
      (is (match-error expected validated)))))

(deftest unique-operation-names
  (testing "duplicate operation name"
    (let [{:keys [validated expected]} (nth cats 30)]
      (is (match-error expected validated))))
  (testing "multiple valid operations with unique names"
    (is (expect-valid (nth cats 31)))))

(deftest unique-input-field-name
  (testing "duplicate input field name on object value"
    (let [{:keys [validated expected]} (nth cats 32)]
      (is (match-error expected validated))))
  (testing "duplicate input field name on schema"
    (let [{:keys [validated expected]} (nth cats 33)]
      (is (match-error expected validated)))))

(deftest unique-fragment-names
  (testing "duplicate fragment name"
    (let [{:keys [validated expected]} (nth cats 34)]
      (is (match-error expected validated)))))

(deftest unique-argument-names
  (testing "duplicate argument name on field (query)"
    (let [{:keys [validated expected]} (nth cats 35)]
      (is (match-error expected validated))))
  (testing "duplicate argument name on type field (schema)"
    (let [{:keys [validated expected]} (nth cats 36)]
      (is (match-error expected validated))))
  (testing "duplicate argument name on directive (query)"
    (let [{:keys [validated expected]} (nth cats 37)]
      (is (match-error expected validated)))))

(deftest provided-non-null-arguments
  (testing "missing required argument"
    (let [{:keys [validated expected]} (nth cats 38)]
      (is (match-error expected validated)))))

(deftest no-unused-variables
  (testing "unused variable"
    (let [{:keys [validated expected]} (nth cats 39)]
      (is (match-error expected validated)))))

(deftest no-unused-fragments
  (testing "unused fragment"
    (let [{:keys [validated expected]} (nth cats 40)]
      (is (match-error expected validated)))))

(deftest known-directives
  (testing "unknown directive"
    (let [{:keys [validated expected]} (nth cats 41)]
      (is (match-error expected validated)))))
