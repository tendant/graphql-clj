(ns graphql-clj.validator-test
  (:require [clojure.test :refer :all]
            [graphql-clj.validator :as validator]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]
            [graphql-clj.test-helpers :as th]))

(def schema
  (-> (parser/parse (slurp "test/scenarios/cats/validation/validation.schema.graphql"))
      validator/validate-schema))

(assert (not (nil? schema)) "No schema found!")

(defn validate-test-case [{:keys [parsed] :as test-case}]
  (assoc test-case :validated (validator/validate-statement parsed schema)))

(def cats
  (->> [(get (yaml/from-file "test/scenarios/cats/validation/DefaultValuesOfCorrectType.yaml") "tests")
        (get (yaml/from-file "test/scenarios/cats/validation/ArgumentsOfCorrectType.yaml") "tests")]
       flatten
       (map th/parse-test-case)
       (map validate-test-case)))

(deftest default-values-of-correct-type
  (testing "default-for-required-field"
    (let [{:keys [validated expected]} (nth cats 3)]
      (is (= (count expected) (-> validated :state :operation-definitions :errors count)))))
  (testing "bad-value-for-default"
    (let [{:keys [validated expected]} (nth cats 4)]
      (is (= (count expected) (->> validated :state :operation-definitions :errors count))))))

(deftest arguments-of-correct-type
  (testing "bad-value"
    (let [{:keys [validated expected]} (nth cats 6)]
      (is (= (count expected) (->> validated :state :operation-definitions :errors count))))))
