(ns cats.scenarios.parsing.schema-parser-test
  (:require [clojure.test :refer :all]
            [yaml.core :as yaml]
            [graphql-clj.parser :as parser]))

(defn- expectation->enum [t]
  (->> (get t "then") keys first keyword))

(defn- interpret-parsed [r]
  (cond (:reason r)                :syntax-error
        (:operation-definitions r) :passes
        :else                      :parser-error))

(defn- parse-query [t]
  (let [q (get-in t ["given" "query"])
        parsed (parser/parse q)]
    {:query    q
     :parsed   parsed
     :expected (expectation->enum t)
     :result   (interpret-parsed parsed)}))

(def schema-parser-tests (get (yaml/from-file "test/cats/scenarios/parsing/SchemaParser.yaml") "tests"))

(assert (not (nil? schema-parser-tests)) "No schema tests found!")

(def parser-results (map parse-query schema-parser-tests))

(deftest schema-parser
  (testing "SchemaParser.yml"
    (doseq [{:keys [expected result] :as r} parser-results]
      (is (= expected result)))))
