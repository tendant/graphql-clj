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
      :else (fn [& args]
              (println (format "Not found resolver function for type(%s) and field(%s)." type-name field-name))
              (fn [context parent & args]
                (get parent (keyword field-name)))))))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [schema (-> simple-user-schema
                     (parser/parse)
                     (parser/transform)
                     (type/create-schema))
          query "query {user {id}}"
          document (parser/transform (parser/parse query))
          context nil]
      (execute context schema resolver-fn document))))
