(ns cats.scenarios.validation.schema-validation-test
  (:require [clojure.test :refer :all]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [cats.scenarios.test-helpers :as th]))

(def schema
  (parser/parse (slurp "test/cats/scenarios/validation/validation.schema.graphql")))

(assert (not (nil? schema)) "No schema found!")

(def schema-validation-tests
  (->> [(get (yaml/from-file "test/cats/scenarios/validation/DefaultValuesOfCorrectType.yaml") "tests")]
       flatten
       (map th/parse-test-case)
       (map (partial th/add-validation schema))))

(assert (not (nil? schema-validation-tests)) "No schema validation tests found!")

(deftest schema-validation
  (testing "parsing DefaultValuesOfCorrectType.yaml"
    (doseq [{:keys [result]} schema-validation-tests]
      (is (= :passes result))))
  #_(testing "validating documents against schema"
      (let [{:keys [expected result]} (nth schema-validation-tests 3)]
        #_(is (= expected result))
        )))

(def required-schema
  "type Person {
     phones: [String!]!
     }")

(def required-schema-expect
  {:operation-definitions   []
   :type-system-definitions [{:type             :type-system-definition
                              :type-system-type :type
                              :name             "Person"
                              :type-fields      [{:name     "phones"
                                                  :type-field-type
                                                            {:kind       :LIST
                                                             :inner-type {:type-field-type {:name "String"}, :required true}}
                                                  :required true}]}]
   :fragments               {}})

(def required-variable
  "query UnreachableDefaultValues($a: Int! = 3, $b: String! = \"default\") {
            dog { name }
            }")

(def required-variable-expect
  {:operation-definitions [{:type           :operation-definition
                            :operation-type {:type "query" :name "UnreachableDefaultValues"}
                            :variable-definitions
                                            [{:name          "a"
                                              :type          {:kind :NON_NULL :inner-type {:named-type "Int"}}
                                              :default-value 3}
                                             {:name          "b"
                                              :type          {:kind :NON_NULL, :inner-type {:named-type "String"}}
                                              :default-value "default"}]
                            :selection-set
                                            [{:selection
                                              {:field
                                               {:name          "dog"
                                                :selection-set [{:selection {:field {:name "name"}}}]}}}]}]
   :type-system-definitions []
   :fragments               {}})


(deftest two-types-of-required
  (testing "required schema"
    (is (= required-schema-expect (parser/parse required-schema))))
  (testing "required variable"
    (is (= required-variable-expect (parser/parse required-variable)))))
