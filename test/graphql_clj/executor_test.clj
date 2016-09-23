(ns graphql-clj.executor-test
  (:use graphql-clj.executor)
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [graphql-clj.introspection :as introspection]
            [clojure.core.match :as match]))

(def simple-user-schema-str
  "type User {
  name: String
  nickname: String
  son: User
  friends: [User]
  phones: [String!]!
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
      ["User" "phones"] (fn [context parent & args]
                          (->> (range 3) (map str) vec))
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

(def simple-user-schema (create-test-schema simple-user-schema-str))

(deftest test-parse-error-execution
  (testing "test parse error execution"
    (let [query "quer {user}"
          result (execute nil simple-user-schema nil query)]
      (is (not (nil? (:error result))))
      (is (= 1 (get-in result [:error :column])))
      (is (= 1 (get-in result [:error :line]))))))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [query "query {user {name}}"
          document (parser/parse query)
          query-operation (first (:operation-definitions document))
          query-selection-set (:selection-set query-operation)
          user-selection (first query-selection-set)
          user-selection-set (get-in user-selection [:selection :field :selection-set])]
      (is (= "user" (get-selection-name user-selection)))
      (is (= :field (get-selection-type user-selection)))
      (is (= user-selection-set (get-field-selection-set user-selection)))
      (is (= [{:selection {:field {:name "name"}}}] (collect-fields user-selection-set nil)))
      (is (= "Test user name" (get-in (execute nil simple-user-schema customized-resolver-fn query) [:data "user" "name"]))))))

(deftest test-execution-on-list
  (testing "test execution on list"
    (let [schema simple-user-schema
          query "query {user {name friends{name}}}"
          context nil]
      (is (= 5 (count (get-in (execute context schema customized-resolver-fn query)
                              [:data "user" "friends"]))))))
  (testing "list of scalars"
    (let [schema simple-user-schema
          query "query {user {phones}}"]
      (is (= ["0" "1" "2"]
             (get-in (execute nil schema customized-resolver-fn query)
                     [:data "user" "phones"]))))))

(deftest test-execution-with-fragment
  (testing "test execution with fragment"
    (let [query "query {user {...userFields friends{...userFields}}}
fragment userFields on User {
  name
  nickname
}"
          context nil]
      (is (= 5 (count (get-in (execute nil simple-user-schema customized-resolver-fn query)
                              [:data "user" "friends"])))))))

(deftest test-mutation
  (testing "test execution on mutation"
    (let [user-name "Mutation Test User"
          mutation (format "mutation {createUser(name: \"%s\") {id name}}" user-name)
          result (execute nil simple-user-schema customized-resolver-fn mutation)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"])))))
  (testing "test execution on mutation with variable"
    (let [user-name "Mutation Test User"
          mutation (format "mutation {createUser(name: $name) {id name}}" user-name)
          variables {:name user-name}
          result (execute nil simple-user-schema customized-resolver-fn mutation variables)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"]))))))
