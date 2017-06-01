(ns graphql-clj.executor-unit-test
  (:require [graphql-clj.executor :as sut :refer :all]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest test-resolve-field-on-object
  (testing "resolve-field-on-object"
    (let [result (@#'sut/resolve-field-value {:resolver-fn (fn [& args]
                                                             "resolved val")
                                              :name "field-name"}
                  {:arguments nil}
                  {}
                  "ParentTypeName"
                  {:vals "Parent result"})]
      (is (= "resolved val" result)))))

