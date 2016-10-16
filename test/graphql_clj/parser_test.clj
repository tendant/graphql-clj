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

(def type-fields-kv-example
  "type Hello {
     world(flag: Boolean = true): String!
     this: Int
   }")

(def input-type-fields-kv-example
  "input Hello {
    world: String!
    this: Int
   }")

(def variable-kv-example
  "query WithDefaultValues(
     $a: Int = 1,
     $b: String! = \"ok\",
     $c: ComplexInput = { requiredField: true, intField: 3 }) {
       dog { name }
     }")

(def enum-argument-example
  "{
    empireHero: hero(episode: EMPIRE) {
      name
    }
    jediHero: hero(episode: JEDI) {
      name
    }
  }")

(deftest parse-kv-pairs
  (testing "we can convert type-fields to a map"
    (is (= (-> (parser/parse type-fields-kv-example) :type-system-definitions first :fields)
           [{:node-type :type-field
             :field-name "world"
             :type-name "String"
             :required true
             :arguments [{:node-type :type-field-argument
                          :argument-name "flag"
                          :type-name "Boolean"
                          :default-value true}]}
            {:node-type :type-field
             :field-name "this"
             :type-name "Int"}])))
  (testing "we can convert input-type-fields to a map"
      (is (= (-> (parser/parse input-type-fields-kv-example) :type-system-definitions first :fields)
             [{:node-type :input-type-field
               :field-name "world"
               :type-name "String"
               :required true}
              {:node-type :input-type-field :field-name "this" :type-name "Int"}])))
  (testing "we can convert variables to a map"
      (is (= (-> (parser/parse variable-kv-example) :operation-definitions first :variable-definitions)
             [{:node-type :variable-definition :variable-name "a" :type-name "Int" :default-value 1}
              {:node-type :variable-definition :variable-name "b" :type-name "String" :required true :default-value "ok"}
              {:node-type :variable-definition :variable-name "c" :type-name "ComplexInput" :default-value [:object-value [{:name "requiredField" :value true}
                                                                                                                           {:name "intField"      :value 3}]]}])))
  (testing "we can convert enum arguments"
    (is (= (-> (parser/parse enum-argument-example) :operation-definitions first :selection-set)
           [{:node-type     :field
             :name          "empireHero"
             :field-name    "hero"
             :arguments
                            [{:node-type     :argument
                              :argument-name "episode"
                              :value         "EMPIRE"}]
             :selection-set [{:node-type :field :field-name "name"}]}
            {:node-type     :field
             :name          "jediHero"
             :field-name    "hero"
             :arguments
                            [{:node-type     :argument
                              :argument-name "episode"
                              :value         "JEDI"}]
             :selection-set [{:node-type :field :field-name "name"}]}]))))

;;;;; Test Helpers for visualizing the parsed tree ;;;;;

(defn render-parsed! [to-filename parsed-data]
  (pp/pprint parsed-data (clojure.java.io/writer (str "test/scenarios/parsed/" to-filename))))

(defn render-all-parsed! []
  (render-parsed! "statements.edn" (mapv parser/parse test-statements))
  (render-parsed! "schemas.edn" (mapv parser/parse test-schemas)))

;; To generate a large parsed tree for inspection from the repl
(comment
  (require '[graphql-clj.parser-test :as pt])
  (pt/render-all-parsed!))
;; Go look in test/scenarios/parsed/schemas.edn and statements.edn
