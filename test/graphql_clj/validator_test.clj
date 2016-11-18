(ns graphql-clj.validator-test
  (:require [clojure.test :refer :all]
            [graphql-clj.validator :as validator]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [graphql-clj.parser-test :as pt]
            [graphql-clj.test-helpers :as th]
            [graphql-clj.validator.spec.statement :as stmt-spec]
            [clojure.spec :as s]))

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

(defn- validate-schema [parsed rules]
  (if rules
    (validator/validate-schema parsed rules)
    (validator/validate-schema parsed)))

(defn- validate-statement [parsed schema rules]
  (if rules
    (validator/validate-statement parsed schema rules)
    (validator/validate-statement parsed schema)))

(defn validate-test-case [{:keys [type parsed rules] :as test-case}]
  (let [validated (if (= :schema type) (validate-schema parsed rules) (validate-statement parsed schema rules))]
    (assoc test-case :validated validated
                     :result (if (-> validated :errors empty?) :passes :errors))))

(def raw-cats
  [(get (yaml/from-file "test/scenarios/cats/validation/ArgumentsOfCorrectType.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/FieldsOnCorrectType.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/KnownArgumentNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/KnownTypeNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/KnownFragmentNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/VariablesAreInputTypes.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/NoUndefinedVariables.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/FragmentsOnCompositeTypes.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/UniqueVariableNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/UniqueOperationNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/UniqueInputFieldNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/UniqueFragmentNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/UniqueArgumentNames.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/ProvidedNonNullArguments.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/NoUnusedVariables.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/NoUnusedFragments.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/KnownDirectives.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/LoneAnonymousOperation.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/VariablesInAllowedPosition.yaml") "tests")
   (get (yaml/from-file "test/scenarios/cats/validation/ScalarLeafs.yaml") "tests")])

(def raw-cats-w-fragment-cycles
  (conj raw-cats (get (yaml/from-file "test/scenarios/cats/validation/NoFragmentCycles.yaml") "tests")))

(def parsed-cats
  (->> raw-cats-w-fragment-cycles flatten (map th/parse-test-case)))

(def cats (map validate-test-case parsed-cats))

(defn- match-error [expected validated]
  (let [errors (->> validated :errors)]
    (cond (keyword? expected) (= expected errors)
          (:loc (first errors)) (= expected errors)
          :else (= (map :error expected) (map :error errors)))))

(defn- expect-valid [{:keys [result expected]}]
  (= expected result))

(deftest validate-cats
  (doseq [{:keys [name result expected validated]} cats]
    (testing name
      (if (= :passes result)
        (is (= expected result))
        (is (match-error expected validated))))))

(def parsed-cats-wo-fragment-cycles
  (->> raw-cats flatten (map th/parse-test-case)))

(deftest validate-statement-output
  (testing "the test output from each validated statement conforms to the spec"
    (doseq [{:keys [type parsed]} parsed-cats-wo-fragment-cycles]
      (when (not= :schema type)
        (let [result (validator/validate-statement parsed schema)]
          (is (s/valid? ::stmt-spec/validation-output result)))))))
