(ns graphql-clj.executor-unit-test
  (:require [graphql-clj.executor :as sut :refer :all]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest test-resolve-field-on-object
  (testing "resolve-field-on-object"
    (let [result (@#'sut/resolve-field-on-object {:resolver-fn (fn [& args]
                                                                 "resolved val")
                                                  :name "field-name"}
                  {:arguments nil}
                  {}
                  "ParentTypeName"
                  {:vals "Parent result"})]
      (is (= {:vals "resolved val"} result)))))

(deftest test-rollup-errors
  (testing "rollup-errors"
    (let [result (@#'sut/rollup-errors [{:errors {:message "error1"}
                                         :vals "vals1"}
                                        {:errors {:message "error2"}
                                         :vals "vals2"}
                                        {:errors {:message "error3"}
                                         :vals "vals3"}])]
      (is (= result {:errors [{:message "error1"} {:message "error2"} {:message "error3"}]
                     :vals ["vals1" "vals2" "vals3"]})))))

(deftest test-complete-value
  (testing "complete-value scalar"
    (let [result (@#'sut/complete-value {:selection-set "selection-set"
                                         :name "name"
                                         :type {:tag :scalar-definition :name "test-type-name"}
                                         :resolved-type "resolved-type"}
                  {:schema "schema"}
                  {:errors [{:message "error 1"}]
                   :vals "vals"})]
      (is (= {:errors [{:message "error 1"}], :vals "vals"} result))))
  
  (testing "complete-value type-definition"
    (let [result (@#'sut/complete-value {:selection-set "selection-set"
                                         :name "name"
                                         :type {:tag :type-definition :name "test-type-name"}
                                         :resolved-type "resolved-type"}
                  {:schema "schema"}
                  {:errors [{:message "error 1"}]
                   :vals "vals"})]
      (is (= result "")))))