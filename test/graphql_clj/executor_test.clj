(ns graphql-clj.executor-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [clojure.core.match :as match]
            [clojure.string :as str]
            [graphql-clj.executor :as executor]
            [graphql-clj.validator :as validator]))

(def borked-user-schema-str
  "type User {
  name: String
  nickname: String
  son: User
  friends: [User]
  phones: [String!]!
}

type QueryRoot {
  user: User
  loremIpsum(words: Int = 1): String!
}

type CreateUser {
  id: String
  name: String
}

type MutationRoot {
  createUser(name: String = \"default user name\", required: Boolean!, optional: String): CreateUser
}

schema {
  query: QueryRoot
  ")

(def simple-user-schema-str
  (str borked-user-schema-str "mutation: MutationRoot
    }"))

(def resolver-fn
  "This resolver is schema / domain specific, implemented at the application level"
  (fn [type-name field-name]
    (match/match
      [type-name field-name]
      ["QueryRoot"  "user"] (fn [& args]
                              {:name "Test user name"
                               :nickname "Test user nickname"})
      ["QueryRoot"  "loremIpsum"] (fn [context parent args]
                                    (let [words (get args "words")]
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
      ["MutationRoot" "createUser"] (fn [context parent arguments]
                                      {:id   (java.util.UUID/randomUUID)
                                       :name (get arguments "name")})
      :else nil)))

(defn- create-test-schema [type-spec]
  (-> type-spec parser/parse validator/validate-schema))

(def invalid-schema (create-test-schema borked-user-schema-str))
(def schema         (create-test-schema simple-user-schema-str))

(defn test-execute [statement-str]
  (-> (executor/prepare schema resolver-fn statement-str) executor/execute))

(deftest parse-error-execution
  (testing "schema parse or validation error in prep phase"
    (let [query-str "query {user}"
          result (executor/prepare invalid-schema resolver-fn query-str)]
      (is (not (nil? (:errors result))))
      (is (= 3 (get-in result [:errors 0 :loc :column])))
      (is (= 25 (get-in result [:errors 0 :loc :line])))))
  (testing "statement parse or validation error in prep phase"
    (let [query-str "quer {user}"
          result (executor/prepare schema resolver-fn query-str)]
      (is (not (nil? (:errors result))))
      (is (= 1 (get-in result [:errors 0 :loc :column])))
      (is (= 1 (get-in result [:errors 0 :loc :line])))))
  (testing "schema parse or validation error prior to execution"
    (let [query-str "query {user}"
          result (executor/execute nil invalid-schema resolver-fn query-str)]
      (is (not (nil? (:errors result))))
      (is (= 3 (get-in result [:errors 0 :loc :column])))
      (is (= 25 (get-in result [:errors 0 :loc :line])))))
  (testing "statement parse or validation error prior to execution"
    (let [query-str "quer {user}"
          result (executor/execute nil schema resolver-fn query-str)]
      (is (not (nil? (:errors result))))
      (is (= 1 (get-in result [:errors 0 :loc :column])))
      (is (= 1 (get-in result [:errors 0 :loc :line]))))))

(deftest backwards-compatibility
  (testing "for unvalidated schemas entering the execution phase"
    (let [query "query {user {name}}"
          result (executor/execute nil (parser/parse simple-user-schema-str) resolver-fn query)]
      (is (not (:errors result)))
      (is (= "Test user name" (get-in result [:data "user" "name"])))))
  (testing "for unvalidated statements entering the execution phase"
    (let [query "query {user {name}}"
          result (executor/execute nil schema resolver-fn query)]
      (is (not (:errors result)))
      (is (= "Test user name" (get-in result [:data "user" "name"]))))))

(deftest simple-execution
  (testing "simple execution"
    (let [result (test-execute "query {user {name}}")]
      (is (not (:errors result)))
      (is (= "Test user name" (get-in result [:data "user" "name"]))))))

(deftest default-argument-value
  (testing "execution of default argument value"
    (let [result (test-execute "query {loremIpsum}")]
      (is (= "Lorem" (get-in result [:data "loremIpsum"]))))))

(deftest field-aliases
  (testing "execution on field aliases"
    (let [result (test-execute "query {loremIpsum(words: 2), threeWords: loremIpsum(words: 3)}")]
      (is (not (:errors result)))
      (is (= {"loremIpsum" "Lorem Lorem"
              "threeWords" "Lorem Lorem Lorem"} (:data result))))))

(deftest execution-on-list
  (testing "execution on list"
    (let [result (test-execute "query {user {name friends{name}}}")]
      (is (not (:errors result)))
      (is (= 5 (count (get-in result [:data "user" "friends"]))))))
  (testing "list of scalars"
    (let [result (test-execute "query {user {phones}}")]
      (is (not (:errors result)))
      (is (= ["0" "1" "2"] (get-in result [:data "user" "phones"]))))))

(deftest execution-with-fragment
  (testing "execution with fragment"
    (let [result (test-execute "query {user {...userFields friends{...userFields}}}
fragment userFields on User {
  name
  nickname
}")]
      (is (not (:errors result)))
      (is (= 5 (count (get-in result [:data "user" "friends"])))))))

(deftest mutation
  (testing "execution on mutation with argument value"
    (let [user-name "Mutation Test User"
          mutation-str (format "mutation {createUser(name: \"%s\", required: true) {id name}}" user-name)
          result (test-execute mutation-str)]
      (is (not (:errors result)))
      (is (= user-name (get-in result [:data "createUser" "name"])))))
  (testing "execution on mutation with variable"
    (let [user-name "Mutation Test User"
          mutation-str (format "mutation($name:String) {createUser(name: $name, required: true) {id name}}" user-name)
          variables {"name" user-name}
          result (executor/execute nil (executor/prepare schema resolver-fn mutation-str) variables)]
      (is (not (:errors result)))
      (is (= user-name (get-in result [:data "createUser" "name"])))))
  (testing "execution on mutation with default argument value"
    (let [user-name "default user name"
          mutation-str (format "mutation {createUser (required: true) {id name}}" user-name)
          result (test-execute mutation-str)]
      (is (not (:errors result)))
      (is (= user-name (get-in result [:data "createUser" "name"]))))))
