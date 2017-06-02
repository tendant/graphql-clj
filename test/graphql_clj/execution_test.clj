(ns graphql-clj.execution-test
  (:require [graphql-clj.executor :as sut]
            [clojure.test :as t :refer :all]
            [clojure.string :as str]
            [graphql-clj.query-validator :as qv]
            [graphql-clj.schema-validator :as sv]))

(def simple-user-schema-str
  "type User {
  name: String
  nickname: String
  son: User
  friends: [User]
  phones: [String!]!
  cannotBeNull: String!
}

type QueryRoot {
  user: User
  loremIpsum(words: Int = 1): String!
  reqArg(arg:Int!): String
  stringList: [String]
  objectList: [User!]!
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
  mutation: MutationRoot
}")

(def user-resolver-fn
  "This resolver is schema / domain specific, implemented at the application level"
  (fn [type-name field-name]
    (get-in {"QueryRoot" {"user" (fn [& args]
                                   {:name     "Test user name"
                                    :nickname "Test user nickname"})
                          "loremIpsum" (fn [context parent args]
                                         (let [words (get args "words")]
                                           (str/join " " (repeat words "Lorem"))))
                          "reqArg" (fn [context parent args] (str (get args "arg")))
                          "stringList" (fn [_ _ _] ["0" "1" "2"])
                          "objectList" (fn [context parent args]
                                         (map (fn [no] {:name (format "Friend %s name" no)
                                                        :nickname (format "Friend %s nickname" no)})
                                              (range 2)))
                          }
             "User" {"son" (fn [context parent args]
                             {:name "Test son name"
                              :nickname "Son's nickname"})
                     "friends" (fn [context parent args]
                                 (map (fn [no] {:name (format "Friend %s name" no)
                                                :nickname (format "Friend %s nickname" no)})
                                      (range 5)))
                     "phones" (fn [context parent args]
                                (->> (range 3) (map str) vec))
                     }
             "MutationRoot" {"createUser" (fn [context parent arguments]
                                            {:id   (java.util.UUID/randomUUID)
                                             :name (get arguments "name")})
                             }}
            [type-name field-name])))

;; (def invalid-schema (create-test-schema borked-user-schema-str))
(def schema (sv/validate-schema simple-user-schema-str))

(defn test-execute
  ([statement-str]
   (sut/execute nil schema user-resolver-fn statement-str))
  ([statement-str variables]
   (sut/execute nil schema user-resolver-fn statement-str variables)))

(deftest missing-variables
  (testing "execution with variables missing"
    (let [query-str "query($wordCount:Int) {loremIpsum(words: $wordCount)}"
          result (test-execute query-str {})]
      (is (seq (:errors result)))
      (is (= [{:message "Missing input variables (wordCount)."}] (:errors result)))))
  (testing "execution with variables missing, but with default values"
    (let [query-str "query($wordCount:Int = 2) {loremIpsum(words: $wordCount)}"
          result (test-execute query-str)]
      (is (empty? (:errors result)))
      (is (= {"loremIpsum" "Lorem Lorem"} (:data result)))
      )))

(deftest backwards-compatibility
  (testing "for unvalidated schemas entering the execution phase"
    (let [query "query {user {name}}"
          result (sut/execute nil simple-user-schema-str user-resolver-fn query)]
      (is (empty? (:errors result)))
      (is (= {:data {"user" {"name" "Test user name"}}}
             result))))
  (testing "for unvalidated statements entering the execution phase"
    (let [query "query {user {name}}"
          result (sut/execute nil schema user-resolver-fn query)]
      (is (empty? (:errors result)))
      (is (= {:data {"user" {"name" "Test user name"}}}
             result)))))

;; FIXME
(deftest simple-execution
  (testing "simple execution"
    (let [result (test-execute "query {user {name}}")]
      (is (not (:errors result)))
      (is (= "Test user name" (get-in result [:data "user" "name"]))))))

(def starwars-schema-str "enum Episode { NEWHOPE, EMPIRE, JEDI }

interface Character {
  id: String!
  name: String
  friends: [Character]
  appearsIn: [Episode]
}

type Human implements Character {
  id: String!
  name: String
  friends: [Character]
  appearsIn: [Episode]
  homePlanet: String
  height: Float
}

type Droid implements Character {
  id: String!
  name: String
  friends: [Character]
  appearsIn: [Episode]
  primaryFunction: String
}

type Query {
  hero(episode: Episode): Character
  human(id: String!): Human
  droid(id: String!): Droid
  testRequiredNoValue: String!
  testScalar: String
  testEnum: Episode
  testInterface: Character
}

type Mutation {
  createHuman(name: String, friends: [String]): Human
}

schema {
  query: Query
  mutation: Mutation
}

input WorldInput {
  text: String
  text2: String
  text3: String
}")

(def luke {:id "1000"
           :name "Luke Skywalker"
           :friends ["1002" "1003" "2000" "2001"]
           :appearsIn [4 5 6]
           :homePlanet "Tatooine"})

(def vader {:id "1001"
            :name "Darth Vader"
            :friends ["1004"]
            :appearsIn [4 5 6]
            :homePlanet "Tatooine"})

(def han {:id "1002"
          :name "Han Solo"
          :friends ["1000" "1003" "2001"]
          :appearsIn [4 5 6]})

(def leia {:id "1003"
           :name "Leia Organa"
           :friends ["1000" "1002" "2000" "2001"]
           :appearsIn [4 5 6]
           :homePlanet "Alderaan"})

(def tarkin {:id "1004"
             :name "Wilhuff Tarkin"
             :friends ["1001"]
             :appearsIn [4]})

(def humanData  (atom {"1000" luke
                       "1001" vader
                       "1002" han
                       "1003" leia
                       "1004" tarkin}))

(def threepio {:id "2000"
               :name "C-3PO"
               :friends ["1000" "1002" "1003" "2001"]
               :appearsIn [4 5 6]
               :primaryFunction "Protocol"})

(def artoo {:id "2001"
            :name "R2-D2"
            :friends ["1000" "1002" "1003"]
            :appearsIn [4 5 6]
            :primaryFunction "Astromech"})

(def droidData (atom {"2000" threepio
                      "2001" artoo}))

(defn get-human [id]
  (get @humanData id)) ; BUG: String should be parsed as string instead of int

(defn get-droid [id]
  (get @droidData id)) ; BUG: String should be parsed as string instead of int

(defn get-character [id]
  (or (get-human id) ; BUG: String should be parsed as string instead of int
      (get-droid id)))

(defn get-friends [character]
  (map get-character (:friends character)))

(defn get-hero [episode]
  (if (= episode 5)
    luke
    artoo))

(def human-id (atom 2050))

(defn create-human [args]
  (let [new-human-id (str (swap! human-id inc))
        new-human {:id new-human-id
                   :name (get args "name")
                   :friends (get args "friends")}]
    (swap! humanData assoc new-human-id new-human)
    new-human))

(defn starwars-resolver-fn [type-name field-name]
  (get-in {"Query" {"hero" (fn [context parent args]
                             (get-hero (str (get args "episode"))))
                    "human" (fn query-human [context parent args]
                              (get-human (str (get args "id"))))
                    "droid" (fn [context parent args]
                              (get-droid (str (get args "id"))))
                    "testScalar" (fn [& args]
                                   "test scalar")
                    "testEnum" (fn [& args]
                                 "JEDI")}
           "Human" {"friends" (fn [context parent args]
                                (get-friends parent))
                    }
           "Droid" {"friends" (fn [context parent args]
                                (get-friends parent))
                    }
           "Character" {"friends" (fn [context parent args]
                                    (get-friends parent))
                        }
           "Mutation" {"createHuman" (fn [context parent args]
                                       (create-human args))}}
          [type-name field-name]))

(def starwars-schema (sv/validate-schema starwars-schema-str))

(deftest test-collect-fields
  (let [query-root-type (get-in schema [:roots :query])]
    (testing "collect fields"
      (let [[errors document] (qv/validate-query starwars-schema "query { hero } ")
            selection-set (:selection-set (first document))
            fields (#'sut/collect-fields query-root-type selection-set {} {})]
        (is (= 1 (count fields)))))
    (testing "collect fields - inline fragment"
      (let [[errors document] (qv/validate-query starwars-schema "query HeroForEpisode($ep: Episode!) {
  hero(episode: $ep) {
    name
    ... on Droid {
      primaryFunction
    }
    ... on Human {
      homePlanet
      height
    }
  }
}")
            selection-set (:selection-set (first (:selection-set (first document))))
            fields (#'sut/collect-fields query-root-type selection-set {} {})]
        (is (empty? errors))
        (is (= 4 (count fields)))))
    ))

(deftest test-execute-fields
  (testing "execute-fields"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { hero }")]
      (is (= [{:message "Object Field(hero) has no selection."}]
             (:errors result))))))

(deftest test-execute-fields-required
  (testing "execute-fields required"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { testRequired }")]
      (is (seq (:errors result))))))

(deftest test-execute-field-scalar
  (testing "execute field scalar"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { testScalar }")]
      (is (empty? (:errors result)))
      (is (= result {:data {"testScalar" "test scalar"}})))))

(deftest test-execute-field-scalar
  (testing "execute field scalar"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { testEnum }")]
      (is (empty? (:errors result)))
      (is (= result {:data {"testEnum" "JEDI"}})))))

(deftest test-execute-field-object-without-selection
  (testing "execute field object"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { human(id: \"1000\") }")]
      (is (seq (:errors result)))
      (is (= result {:errors [{:message "Object Field(human) has no selection."}]
                     :data {"human" nil}})))))

(deftest test-execute-field-object
  (testing "execute field object"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { human(id: \"1000\") { id name } }")]
      (is (empty? (:errors result)))
      (is (= {:data {"human" {"id" "1000"
                              "name" "Luke Skywalker"}}}
             result)))))

(deftest test-execute-field-interface
  (testing "execute field interface"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { hero (episode: \"EMPIRE\") { id name } }")]
      (is (empty? (:errors result)))
      (is (= {:data {"hero" {"id" "2001", "name" "R2-D2"}}}
             result)))))

(deftest test-execute-field-list
  (testing "execute field object"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query { human(id: \"1000\") { id name friends } }")]
      (is (seq (:errors result)))
      (is (= {:errors [{:message "Object Field(friends) has no selection."}],
              :data {"human" {"id" "1000", "name" "Luke Skywalker", "friends" ()}}}
             result)))))

(deftest test-execute-multiple-queries
  (testing "execute multiple queries"
    (let [result (sut/execute nil starwars-schema starwars-resolver-fn
                              "query a { human(id: \"1000\") { id name friends } }
                               query b { human(id: \"1001\") { id name friends } }")]
      (is (seq (:errors result)))
      (is (= {:errors
              [{:message "Must provide operation name if query contains multiple operations."}]}
             result)))))

