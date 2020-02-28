(ns graphql-clj.argument-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [clojure.core.match :as match]
            [clojure.string :as str]
            [graphql-clj.executor :as executor]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.schema-validator :as sv]
            [graphql-clj.query-validator :as qv]))

(def schema-str "type User {
  id: String
  name: String
  age: Int
}

type QueryRoot {
  user: User
}

type MutationRoot {
  updateUser(id: String): User
}

schema {
  query: QueryRoot,
  mutation: MutationRoot,
}")

(defn resolver-fn [type-name field-name]
  (get-in {"MutationRoot" {"updateUser" (fn [context parent args]
                                        {:id (get args "id")
                                        :name "test user name"
                                        :age 30})}
           "QueryRoot" {"user" (fn [context parent args]
                                 {:id "1001"
                                   :name "test user name"
                                  :age 30})}}
          [type-name field-name]))

(require '[graphql-clj.executor :as executor])

(def query-str "mutation { updateUser() {id}}")


(deftest test-execute-mutation-with-nil
  (testing "execute field scalar"
    (let [result (executor/execute nil schema-str resolver-fn
                              "mutation { updateUser() {id}}")]
      (is (empty? (:errors result)))
      (is (= result {:data {"updateUser" {"id" nil}}})))))


(deftest test-execute-mutation-with-string
  (testing "execute field scalar"
    (let [result (executor/execute nil schema-str resolver-fn
                              "mutation { updateUser(id: \"1001\") {id}}")]
      (is (empty? (:errors result)))
      (is (= result {:data {"updateUser" {"id" "1001"}}})))))
            

