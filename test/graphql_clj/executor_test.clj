(ns graphql-clj.executor-test
  (:use graphql-clj.executor)
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.introspection :as introspection]
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

type CreateUser {
  id: String
  name: String
}

type Mutation {
  createUser: CreateUser
}

schema {
  query: QueryRoot
  mutation: Mutation
}")

(def customized-resolver-fn
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
     ["Mutation" "createUser"] (fn [context parent & rest]
                                 (let [arguments (first rest)]
                                   {:id (java.util.UUID/randomUUID)
                                    :name (get arguments "name")}))
     :else nil)))

(defn- create-test-schema
  [type-spec]
  (-> type-spec
      (parser/parse)
      (type/create-schema (parser/parse introspection/introspection-system))))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [schema (create-test-schema simple-user-schema)
          query "query {user {name}}"
          document (parser/parse query)
          context nil
          query-operation (first (:operation-definitions document))
          query-selection-set (:selection-set query-operation)
          user-selection (first query-selection-set)
          user-selection-set (get-in (second user-selection) [:field :selection-set])
          new-result (execute context schema customized-resolver-fn query)
          ]
      (is (= "user" (get-selection-name user-selection)))
      (is (= :field (get-selection-type user-selection)))
      (is (= user-selection-set (get-field-selection-set user-selection)))
      (is (= [[:selection {:field {:name "name"}}]] (collect-fields user-selection-set nil)))
      (is (= "Test user name" (get-in (execute context schema customized-resolver-fn query) [:data "user" "name"])))
      )))

(deftest test-execution-on-list
  (testing "test execution on list"
    (let [schema (create-test-schema simple-user-schema)
          query "query {user {name friends{name}}}"
          context nil]
      (is (= 5 (count (get-in (execute context schema customized-resolver-fn query)
                              [:data "user" "friends"])))))))

(deftest test-execution-with-fragment
  (testing "test execution with fragment"
    (let [schema (create-test-schema simple-user-schema)
          query "query {user {...userFields friends{...userFields}}}
fragment userFields on User {
  name
  nickname
}"
          context nil]
      (is (= 5 (count (get-in (execute context schema customized-resolver-fn query)
                              [:data "user" "friends"])))))))

(deftest test-mutation
  (testing "test execution on mutation"
    (let [schema (create-test-schema simple-user-schema)
          user-name "Mutation Test User"
          mutation (format "mutation {createUser(name: \"%s\") {id name}}" user-name)
          context nil
          result (execute context schema customized-resolver-fn mutation)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"])))))
  (testing "test execution on mutation with variable"
    (let [schema (create-test-schema simple-user-schema)
          user-name "Mutation Test User"
          mutation (format "mutation {createUser(name: $name) {id name}}" user-name)
          context nil
          variables {"name" user-name}
          result (execute context schema customized-resolver-fn mutation variables)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"]))))))
