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

(defn validate-test-case [{:keys [type parsed rules] :as test-case}]
  (let [validated (if (= :schema type)
                    (if rules
                      (validator/validate-schema parsed rules)
                      (validator/validate-schema parsed))
                    (if rules
                      (validator/validate-statement parsed schema rules)
                      (validator/validate-statement parsed schema)))]
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
        (get (yaml/from-file "test/scenarios/cats/validation/KnownDirectives.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/LoneAnonymousOperation.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/VariablesInAllowedPosition.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/ScalarLeafs.yaml") "tests")]
       flatten
       (map th/parse-test-case)
       (map validate-test-case)))

(defn- match-error [expected validated]
  (= (if (keyword? expected) expected (map :error expected))
     (->> validated :state :errors (map :error))))

(defn- expect-valid [{:keys [result expected]}]
  (= expected result))

(deftest validate-cats
  (doseq [{:keys [name result expected validated]} cats]
    (testing name
      (if (= :passes result)
        (is (= expected result))
        (is (match-error expected validated))))))
