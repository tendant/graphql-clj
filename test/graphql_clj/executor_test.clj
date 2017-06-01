(ns graphql-clj.executor-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [clojure.core.match :as match]
            [clojure.string :as str]
            [graphql-clj.executor :as executor]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.schema-validator :as sv]
            [graphql-clj.query-validator :as qv]))

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
    (match/match
      [type-name field-name]
      ["QueryRoot"  "user"] (fn [& args]
                              {:name     "Test user name"
                               :nickname "Test user nickname"})
      ["QueryRoot"  "loremIpsum"] (fn [context parent args]
                                    (let [words (get args "words")]
                                      (str/join " " (repeat words "Lorem"))))
      ["QueryRoot" "reqArg"] (fn [context parent args] (str (get args "arg")))
      ["QueryRoot" "stringList"] (fn [_ _ _] ["0" "1" "2"])
      ["QueryRoot" "objectList"] (fn [context parent args]
                                   (map (fn [no] {:name (format "Friend %s name" no)
                                                  :nickname (format "Friend %s nickname" no)})
                                        (range 2)))
      ["User" "son"] (fn [context parent args]
                       {:name "Test son name"
                        :nickname "Son's nickname"})
      ["User" "friends"] (fn [context parent args]
                           (map (fn [no] {:name (format "Friend %s name" no)
                                          :nickname (format "Friend %s nickname" no)})
                                (range 5)))
      ["User" "phones"] (fn [context parent args]
                          (->> (range 3) (map str) vec))
      ["User" "cannotBeNull"] (constantly nil)
      ["MutationRoot" "createUser"] (fn [context parent arguments]
                                      {:id   (java.util.UUID/randomUUID)
                                       :name (get arguments "name")})
      :else nil)))


;; (def invalid-schema (create-test-schema borked-user-schema-str))
(def schema (sv/validate-schema simple-user-schema-str))

(defn- prepare-statement* [statement-str]
  (let [resolver-fn (resolver/create-resolver-fn schema user-resolver-fn)
        result (qv/validate-query schema statement-str)]
    ;; (assert )
    result))

;; (def prepare-statement (memoize prepare-statement*))
(def prepare-statement prepare-statement*)

(defn test-execute
  ([statement-str]
   (executor/execute nil schema user-resolver-fn (prepare-statement statement-str)))
  ([statement-str variables]
   (executor/execute nil schema user-resolver-fn (prepare-statement statement-str) variables)))

;; (deftest parse-error-execution
;;   (testing "schema parse or validation error"
;;     (let [query-str "query {user}"
;;           result (executor/execute nil invalid-schema user-resolver-fn query-str)]

;;       (is (not (nil? (:errors result))))
;;       (is (= 3 (get-in result [:errors 0 :loc :column])))
;;       (is (= 29 (get-in result [:errors 0 :loc :line])))))
;;   (testing "statement parse or validation error"
;;     (let [query-str "quer {user}"
;;           result (test-execute query-str)]
;;       (is (not (nil? (:errors result))))
;;       (is (= 1 (get-in result [:errors 0 :loc :column])))
;;       (is (= 1 (get-in result [:errors 0 :loc :line]))))))

(deftest backwards-compatibility
  (testing "for unvalidated schemas entering the execution phase"
    (let [query "query {user {name}}"
          result (executor/execute nil (-> simple-user-schema-str parser/parse-schema sv/validate-schema) user-resolver-fn query)]
      (is (not (:errors result)))
      
      (is (= "Test user name" (get-in result [:data "user" "name"])))))
  (testing "for unvalidated statements entering the execution phase"
    (let [query "query {user {name}}"
          result (executor/execute nil schema user-resolver-fn query)]
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

(deftest variable-arguments
  (testing "execution on field arguments with variable bindings"
    (let [query-str "query($n:Int) {loremIpsum(words: $n)}"
          result (test-execute query-str {"n" 5})]
      (is (not (:errors result)))
      (is (= {"loremIpsum" "Lorem Lorem Lorem Lorem Lorem"} (:data result)))))
  (testing "execution on required field arguments with variable bindings"
    (let [query-str "query($n:Int!) {reqArg(arg:$n)}"
          result (test-execute query-str {"n" 5})]
      (is (not (:errors result)))
      (is (=  {"reqArg" "5"} (:data result))))))

(deftest missing-variables
  (testing "execution with variables missing"
    (let [query-str "query($wordCount:Int) {loremIpsum(words: $wordCount)}"
          result (test-execute query-str {})]
      (is (= [{:message "Missing input variables (wordCount)."}] (:errors result)))))
  (testing "execution with variables missing, but with default values"
    (let [query-str "query($wordCount:Int = 2) {loremIpsum(words: $wordCount)}"
          result (test-execute query-str)]
      (is (not (:errors result)))
      (is (= {"loremIpsum" "Lorem Lorem"} (:data result))))))

(deftest execution-on-list
  (testing "execution on list"
    (let [result (test-execute "query {user {name friends{name}}}")]
      (is (not (:errors result)))
      (is (= 5 (count (get-in result [:data "user" "friends"]))))))
  (testing "list of scalars"
    (let [result (test-execute "query {user {phones}}")]
      (is (not (:errors result)))
      (is (= ["0" "1" "2"] (get-in result [:data "user" "phones"])))))
  (testing "execution when root type is a list"
    (let [result (test-execute "query {stringList}")]
      (is (not (:errors result)))
      (is (= {"stringList" ["0" "1" "2"]} (:data result)))))
  (testing "execution when root type is a list of not null objects"
    (let [result (test-execute "query {objectList {name}}")]
      (is (not (:errors result)))
      (is (= {"objectList" [{"name" "Friend 0 name"} {"name" "Friend 1 name"}]} (:data result))))))

(deftest execution-with-fragment
  (testing "execution with fragment"
    (let [query-str "query {user {...userFields friends{...userFields}}}
fragment userFields on User {
  name
  nickname
}"
          result (test-execute query-str)]
      (is (not (:errors result)))
      (is (= 5 (count (get-in result [:data "user" "friends"]))))))
  (testing "execution with inline fragments"
    (let [query-str "query {user {... on User { name nickname } friends{... on User { name nickname }}}}"
          result (test-execute query-str)]
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
          result (test-execute mutation-str variables)]
      (is (not (:errors result)))
      (is (= user-name (get-in result [:data "createUser" "name"])))))
  (testing "execution on mutation with default argument value"
    (let [user-name "default user name"
          mutation-str (format "mutation {createUser (required: true) {id name}}" user-name)
          result (test-execute mutation-str)]
      (is (not (:errors result)))
      (is (= user-name (get-in result [:data "createUser" "name"]))))))

(deftest readme-example
  (let [schema-str "type User {
    name: String
    age: Int
  }
  type QueryRoot {
    user: User
  }

  schema {
    query: QueryRoot
  }"

        type-schema (sv/validate-schema schema-str)
        resolver-fn (fn [type-name field-name]
                      (cond
                        (and (= "QueryRoot" type-name) (= "user" field-name)) (fn [_context _parent _args]
                                                                                {:name "test user name"
                                                                                 :age  30})))
        query-str "query {user {name age}}"
        context nil
        query (qv/validate-query type-schema query-str)]
    (testing "the code in the README works for pre-validation / memoization"
      (is (= {:data {"user" {"name" "test user name" "age" 30}}}
             (executor/execute context type-schema resolver-fn query))))
    (testing "the code in the README works for backwards compatibility (executing string queries)"
      (is (= {:data {"user" {"name" "test user name" "age" 30}}}
             (executor/execute context type-schema resolver-fn query-str))))))

(deftest readme-error-example
  (let [schema-str "type User {
    name: String
    age: Int
  }
  type QueryRoot {
    user: User
  }

  schema {
    query: QueryRoot
  }"

        type-schema (sv/validate-schema schema-str)
        resolver-fn (fn [type-name field-name]
                      (cond
                        (and (= "QueryRoot" type-name) (= "user" field-name)) (fn [_context _parent _args]
                                                                                {:name "test user name"
                                                                                 :age  30})))
        query-str "mutation {user {name age}}"
        context nil
        query (qv/validate-query type-schema query-str)]
    (testing "the code in the README works for pre-validation / memoization"
      (is (= {:errors
              [{:message "schema does not define a root 'mutation' type",
                :start {:line 1, :column 1, :index 0},
                :end {:line 1, :column 27, :index 26}}]}
             (executor/execute context type-schema resolver-fn query))))))

;;; FIXME
;; (deftest null-result-for-non-null-type
;;   (testing "we return an error if we receive an invalid nil value"
;;     (let [result (test-execute "query {user {cannotBeNull}}")] 
;;       (is (= ["NOT_NULL field \"cannotBeNull\" assigned a null value."] (:errors result))))))

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
  (match/match
    [type-name field-name]
    ["Query" "hero"] (fn [context parent args]
                       (get-hero (:episode args)))
    ["Query" "human"] (fn [context parent args]
                        (get-human (str (get args "id"))))
    ["Query" "droid"] (fn [context parent args]
                        (get-droid (str (get args "id"))))
    ;; Hacky!!! Should use resolver for interface
    ["Human" "friends"] (fn [context parent args]
                          (get-friends parent))
    ["Droid" "friends"] (fn [context parent args]
                          (get-friends parent))
    ["Character" "friends"] (fn [context parent args]
                              (get-friends parent))
    ["Mutation" "createHuman"] (fn [context parent args]
                                 (create-human args))
    :else nil))

(def valid-starwars-schema (sv/validate-schema starwars-schema-str))

(defn- prepare-starwars-statement* [statement-str]
  (qv/validate-query valid-starwars-schema statement-str))

(def prepare-starwars-statement prepare-starwars-statement*)

(deftest starwars-example-query
  (testing "we can run the Starwars query from the starter project"
    (let [context nil
          query "query {\n  human (id:\"1002\") {\n    id\n    name\n    friends {\n      id\n      name\n      friends {\n        id\n      }\n    }\n  }\n}"
          variables nil
          validated-statement (prepare-starwars-statement query)]
      ;; (assert (s/valid? ::stmt-spec/validation-output validated-statement))
      (is (= {:data {"human" {"id"      "1002"
                              "name"    "Han Solo"
                              "friends" [{"id"      "1000"
                                          "name"    "Luke Skywalker"
                                          "friends" [{"id" "1002"} {"id" "1003"} {"id" "2000"} {"id" "2001"}]}
                                         {"id"      "1003"
                                          "name"    "Leia Organa"
                                          "friends" [{"id" "1000"} {"id" "1002"} {"id" "2000"} {"id" "2001"}]}
                                         {"id" "2001"
                                          "name" "R2-D2"
                                          "friends" [{"id" "1000"} {"id" "1002"} {"id" "1003"}]}]}}}
             (executor/execute context valid-starwars-schema starwars-resolver-fn validated-statement variables))))))

(def mutation-schema (sv/validate-schema "type Query {
  people: String
}

type Mutation {
  createPeople(emails: [String]): String
}

schema {
  query: Query
  mutation: Mutation
}"))

(deftest list-type-variables
  (testing "validation for list type variable"
    (let [query-str "mutation($emails: [String]) {
  createPeople(emails: $emails)
}"
          result (executor/execute nil mutation-schema (constantly nil) query-str {"emails" ["this@that.com"]})]
      (is (not (:errors result))))))

(def test-introspection-query "query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  }")

(deftest introspection-query
  (testing "introspection query"
    (let [validated-query (qv/validate-query valid-starwars-schema test-introspection-query)
          result (executor/execute nil valid-starwars-schema (constantly nil) validated-query nil)]
      (is (nil? (:errors result)))
      (is (not (nil? (:data result))))
      (is (not (nil? (get-in result [:data "__schema"]))))
      (let [types (get-in result [:data "__schema" "types"])
            episode-type (filter #(= 'Episode (get % "name")) types)]
        (is (not (nil? types)))
        (is (= 1 (count episode-type)))
        (is (= 3 (count (get (first episode-type) "enumValues")))))
      (let [types (get-in result [:data "__schema" "types"])
            world-input (filter #(= 'WorldInput (get % "name")) types)]
        (is (not (nil? types)))
        (is (= 1 (count world-input)))
        (is (= 3 (count (get (first world-input) "inputFields")))))
      )))

(defn list-type-argument-mutation-resolver [type-name field-name]
  (get-in {"Mutation" {"createPerson" (fn [context parent args]
                                        {:id "1"
                                         :name (get args "name")
                                         :friends (map (fn [id] {:name (str "friend " id)}) (get args "friends"))})}}
          [type-name field-name]))

(deftest list-type-argument-mutation
  (testing "list type argument mutation: github issue #53"
    (let [schema-str "type Person {
  id: String!
  name: String
  friends: [Person]
}

type Mutation {
  createPerson(name: String, friends: [String]): Person
}

type Query {
}

schema {
 query: Query
  mutation: Mutation
}"
          mutation-str "mutation {
  createPerson(name: \"John\", friends: [\"1\" \"2\"]) {
    name,
    id,
    friends {
      name
    }
  }
}"
          result (executor/execute nil schema-str list-type-argument-mutation-resolver mutation-str)]
      (is (:data result))
      (is (= (list {"name" "friend 1"}
                   {"name" "friend 2"}) (get-in result [:data "createPerson" "friends"]))))))

(defmacro deftest-execution [name schema resolver-fn query expected]
  `(deftest ~name
     (let [expect# ~expected
           {:keys [data# errors#] :as actual#} (executor/execute nil ~schema ~resolver-fn ~query)]
       (if (or (nil? expect#) (= expect# actual#))
         (report {:type :pass})
         (report {:type :fail :expected expect# :actual actual#})))))

(deftest-execution execution-required
  "type QueryRoot {
  name: String!
}"
  (constantly nil)
  "query { name }"
  {:errors [{:message "NOT_NULL field \"name\" assigned a null value."}]
   :data {"name" nil}})
