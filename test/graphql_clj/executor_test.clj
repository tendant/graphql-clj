(ns graphql-clj.executor-test
  (:use graphql-clj.executor)
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [clojure.core.match :as match]))

(def simple-user-schema
  "type User {
  name: String
  nickname: String
  son: User
  friends: [User]
}

type QueryRoot {
  user: User
}

schema {
  query: QueryRoot
}")

(def resolver-fn 
  (fn [type-name field-name]
    (match/match
     [type-name field-name]
     ["QueryRoot"  "user"] (fn [& args]
                             {:name "Test user name"
                              :nickname "Test user nickname"})
     ["User" "son"] (fn [context parent & args]
                      {:name "Test son name"
                       :nickname "Son's nickname"})
     ["User" "friends"] (fn [context parent & args]
                          (map (fn [no] {:name "Friend 1 name"
                                        :nickname "Friend 1 nickname"})
                               (range 5)))
     :else (fn [context parent & args]
             (println (format "Not found resolver function for type(%s) and field(%s)." type-name field-name))
             (get parent (keyword field-name))))))

(defn- create-test-schema
  [type-spec]
  (-> type-spec
      (parser/parse)
      (parser/transform)
      (type/create-schema)))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [schema (create-test-schema simple-user-schema)
          query "query {user {name}}"
          document (parser/transform (parser/parse query))
          context nil
          query-operation (first (:operation-definitions document))
          query-selection-set (:selection-set query-operation)
          user-selection (first query-selection-set)
          user-selection-set (get-in (second user-selection) [:field :selection-set])
          new-document (parser/transform (parser/parse "query {user {name son}}"))
          new-result (execute context schema resolver-fn new-document)
          ]
      (is (= "user" (get-selection-name user-selection)))
      (is (= :field (get-selection-type user-selection)))
      (is (= user-selection-set (get-field-selection-set user-selection)))
      (is (= [[:selection {:field {:name "name"}}]] (collect-fields user-selection-set nil)))
      (is (= "Test user name" (get-in (execute context schema resolver-fn document) [:data "user" "name"])))
      )))

(deftest test-list-execution
  (testing "test execution on list"
    (let [schema (create-test-schema simple-user-schema)
          query "query {user {name friends{name}}}"
          document (parser/transform (parser/parse query))
          context nil]
      (is (= 5 (count (get-in (execute context schema resolver-fn document)
                              [:data "user" "friends"])))))))
