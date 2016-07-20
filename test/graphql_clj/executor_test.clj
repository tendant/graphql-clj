(ns graphql-clj.executor-test
  (:use graphql-clj.executor)
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]))

(def simple-user-schema
  "type User {
  name: String
  nickname: String
}

type QueryRoot {
  user: User
}

schema {
  query: QueryRoot
}")

(def resolver-fn 
  (fn [type-name field-name]
    (cond
      (and (= type-name "QueryRoot") (= field-name "user")) (fn [& args]
                                                              {:name "Test user name"
                                                               :nickname "Test user nickname"})
      :else (fn [context parent & args]
              (println (format "Not found resolver function for type(%s) and field(%s)." type-name field-name))
              (get parent (keyword field-name))))))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [schema (-> simple-user-schema
                     (parser/parse)
                     (parser/transform)
                     (type/create-schema))
          query "query {user {name}}"
          document (parser/transform (parser/parse query))
          context nil
          query-operation (first (:operation-definitions document))
          query-selection-set (:selection-set query-operation)
          user-selection (first query-selection-set)
          user-selection-set (get-in (second user-selection) [:field :selection-set])]
      (is (= "user" (get-selection-name user-selection)))
      (is (= :field (get-selection-type user-selection)))
      (is (= user-selection-set (get-field-selection-set user-selection)))
      (is (= [[:selection {:field {:name "name"}}]] (collect-fields user-selection-set nil)))
      (is (= "Test user name" (get-in (execute context schema resolver-fn document) [:data "user" "name"])))
      )))
