(ns graphql-clj.executor-test
  (:use graphql-clj.executor)
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [graphql-clj.introspection :as introspection]
            [clojure.core.match :as match]
            [clojure.string :as str]
            [graphql-clj.box :as box]))

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
  loremIpsum(words: Int! = 1): String!
}

type CreateUser {
  id: String
  name: String
}

type Mutation {
  createUser(name: String = \"default user name\", required: Boolean!, optional: String): CreateUser
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
      ["QueryRoot"  "loremIpsum"] (fn [context parent args]
                                    (let [words (box/box->val (get args "words"))]
                                      (str/join " " (repeat words "Lorem"))))
      ["User" "son"] (fn [context parent args]
                       {:name "Test son name"
                        :nickname "Son's nickname"})
      ["User" "friends"] (fn [context parent args]
                           (map (fn [no] {:name (format "Friend %s name" no)
                                          :nickname (format "Friend %s nickname" no)})
                                (range 5)))
      ["User" "phones"] (fn [context parent args]
                          (->> (range 3) (map str) vec))
      ["Mutation" "createUser"] (fn [context parent arguments]
                                  {:id   (java.util.UUID/randomUUID)
                                   :name (box/box->val (get arguments "name"))})
      :else nil)))

(defn- create-test-schema
  [type-spec]
  (-> type-spec
      (parser/parse)
      (type/create-schema introspection/introspection-schema)))

(def simple-user-schema (create-test-schema simple-user-schema-str))

(deftest test-parse-error-execution
  (testing "test parse error execution"
    (let [query "quer {user}"
          result (execute nil simple-user-schema nil query)]
      (is (not (nil? (:errors result))))
      (is (= 1 (get-in result [:errors 0 :column])))
      (is (= 1 (get-in result [:errors 0 :line]))))))

(deftest test-simple-execution
  (testing "test simple execution"
    (let [query "query {user {name}}"
          document            (parser/parse query)
          query-operation     (first (:operation-definitions document))
          query-selection-set (:selection-set query-operation)
          user-selection      (first query-selection-set)
          user-selection-set  (:selection-set user-selection)]
      (is (= "user" (get-selection-name user-selection)))
      (is (= :field (:node-type user-selection)))
      (is (= user-selection-set (:selection-set user-selection)))
      (is (= [{:node-type :field :field-name "name"}] (collect-fields user-selection-set nil)))
      (is (= "Test user name" (get-in (execute nil simple-user-schema customized-resolver-fn query) [:data "user" "name"]))))))

(deftest test-default-argument-value
  (testing "test execution of default argument value"
    (let [schema simple-user-schema
          query "query {loremIpsum}"
          context nil]
      (is (= "Lorem" (-> (execute context schema customized-resolver-fn query)
                         :data
                         (get "loremIpsum")))))))

(deftest test-alias
  (testing "test execution on alias"
    (let [schema simple-user-schema
          query "query {loremIpsum(words: 2), threeWords: loremIpsum(words: 3)}"
          context nil]
      (is (= {"loremIpsum" "Lorem Lorem"
              "threeWords" "Lorem Lorem Lorem"}
             (:data (execute context schema customized-resolver-fn query)))))))

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
  (testing "test execution on mutation with argument value"
    (let [user-name "Mutation Test User"
          mutation (format "mutation {createUser(name: \"%s\", required: true) {id name}}" user-name)
          result (execute nil simple-user-schema customized-resolver-fn mutation)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"])))))
  (testing "test execution on mutation with variable"
    (let [user-name "Mutation Test User"
          mutation (format "mutation($name:String) {createUser(name: $name, required: true) {id name}}" user-name)
          variables {"name" user-name}
          result (execute nil simple-user-schema customized-resolver-fn mutation variables)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"])))))
  (testing "test execution on mutation with default argument value"
    (let [user-name "default user name"
          mutation (format "mutation {createUser (required: true) {id name}}" user-name)
          variables nil
          result (execute nil simple-user-schema customized-resolver-fn mutation variables)]
      (is (= user-name (get-in result
                               [:data "createUser" "name"]))))))
