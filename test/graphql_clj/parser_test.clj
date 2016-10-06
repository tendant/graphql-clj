(ns graphql-clj.parser-test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [graphql-clj.parser :refer :all]))

(def test-statements
  [
   "query {
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50.0)
  }
}
"
   
   "query {
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}"

   "{
  user(id: 4) {
    name
  }
}"

   "mutation {
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}"

   "{
  me {
    id
    firstName
    lastName
    birthday {
      month
      day
    }
    friends {
      name
    }
  }
}"

   "# `me` could represent the currently logged in viewer.
{
  me {
    name
  }
}
"

   "# `me` could represent the currently logged in viewer.
{
  me {
    name
  }
}

# `user` represents one of many users in a graph of data, referred to by a
# unique identifier.
{
  user(id: 4) {
    name
  }
}"

   "{
  user(id: 4) {
    id
    name
    profilePic(size: 100)
  }
}
"
   "{
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}
"

   ;; In this example, we can fetch two profile pictures of different sizes and ensure the resulting object will not have duplicate keys:
   "{
  user(id: 4) {
    id
    name
    smallPic: profilePic(size: 64)
    bigPic: profilePic(size: 1024)
  }
}
"

   ;; Since the top level of a query is a field, it also can be given an alias:
   "{
  zuck: user(id: 4) {
    id
    name
  }
}
"

   ;; Fragments allow for the reuse of common repeated selections of fields, reducing duplicated text in the document. Inline Fragments can be used directly within a selection to condition upon a type condition when querying against an interface or union.
   
   ;; For example, if we wanted to fetch some common information about mutual friends as well as friends of some user:
   "query noFragments {
  user(id: 4) {
    friends(first: 10) {
      id
      name
      profilePic(size: 50)
    }
    mutualFriends(first: 10) {
      id
      name
      profilePic(size: 50)
    }
  }
}"

   ;; The repeated fields could be extracted into a fragment and composed by a parent fragment or query.
   "query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}

fragment friendFields on User {
  id
  name
  profilePic(size: 50)
}
"

   ;; Fragments are consumed by using the spread operator (...). All fields selected by the fragment will be added to the query field selection at the same level as the fragment invocation. This happens through multiple levels of fragment spreads.

   ;; For example:
   "query withNestedFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}

fragment friendFields on User {
  id
  name
  ...standardProfilePic
}

fragment standardProfilePic on User {
  profilePic(size: 50)
}
"

   "query FragmentTyping {
  profiles(handles: [\"zuck\", \"cocacola\"]) {
    handle
    ...userFragment
    ...pageFragment
  }
}

fragment userFragment on User {
  friends {
    count
  }
}

fragment pageFragment on Page {
  likers {
    count
  }
}"

   "query inlineFragmentTyping {
  profiles(handles: [\"zuck\", \"cocacola\"]) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}"

   "query HeroForEpisode($ep: Episode!) {
   hero(episode: $ep) {
   name
   ... on Droid {
         primaryFunction
       }
     }
   }"

   "query inlineFragmentNoType($expandedInfo: Boolean) {
  user(handle: \"zuck\") {
    id
    name
    ... @include(if: $expandedInfo) {
      firstName
      lastName
      birthday
    }
  }
}"

   "query getZuckProfile($devicePicSize: Int) {
  user(id: 4) {
    id
    name
    profilePic(size: $devicePicSize)
  }
}"

   "query hasConditionalFragment($condition: Boolean) {
  ...maybeFragment @include(if: $condition)
}

fragment maybeFragment on Query {
  me {
    name
  }
}"

   "query hasConditionalFragment($condition: Boolean) {
  ...maybeFragment
}

fragment maybeFragment on Query @include(if: $condition) {
  me {
    name
  }
}"

   "query myQuery($someTest: Boolean) {
  experimentalField @skip(if: $someTest)
}"

   "mutation setName {
  setName(name: \"Zuck\") {
    newName
  }
}"

"{
   human(id: 1000) {
     name
     height(unit: FOOT)
     weight(above:100)
   }
}"])

(deftest test-parse
  (doseq [statement test-statements]
    (testing (str "Test all statements parsing, statement: " statement)
      (is (not (insta/failure? (parse statement)))))))

(def test-schemas
  ["type Person {
  name: String
  age: Int
  picture: Url
}
"
   "type Person {
  name(id: ID): String
  age: Int
  picture: Url
}"
   "type Person {
  name(id: ID id2: ID): String
  age: Int
  picture: Url
}"
   "type Person {
  name: String
  age: Int
}

type Photo {
  height: Int
  width: Int
}
"
   "type Person {
  name: String
  age: Int
}

type Photo {
  height: Int
  width: Int
}

type SearchQuery {
  firstSearchResult: SearchResult
}"
   "type Person {
  name: String
  age: Int
  picture: Url
  relationship: Person
}"
   "interface NamedEntity {
  name: String
}

type Person implements NamedEntity {
  name: String
  age: Int
}

type Business implements NamedEntity {
  name: String
  employeeCount: Int
}"
   "union SearchResult = Photo | Person

type Person {
  name: String
  age: Int
}

type Photo {
  height: Int
  width: Int
}

type SearchQuery {
  firstSearchResult: SearchResult
}"
   
   (slurp "test/cats/scenarios/validation/validation.schema.graphql")

   "schema {
  query: Query
  mutation: Mutation
}"

"enum Episode {
   NEWHOPE
   EMPIRE
   JEDI
}"

"type Starship {
  id: ID!
  name: String!
  length(unit: LengthUnit = METER): Float
}"

"type Human implements Character {
  id: ID!
  name: String!
  friends: [Character]
  appearsIn: [Episode]!
  starships: [Starship]
  totalCredits: Int
 }

 type Droid implements Character {
   id: ID!
   name: String!
   friends: [Character]
   appearsIn: [Episode]!
   primaryFunction: String
 }"
 "{
    empireHero: hero(episode: EMPIRE) {
      name
    }
    jediHero: hero(episode: JEDI) {
      name
    }
  }"
 "{
    leftComparison: hero(episode: EMPIRE) {
      ...comparisonFields
    }
    rightComparison: hero(episode: JEDI) {
      ...comparisonFields
    }
  }

  fragment comparisonFields on Character {
    name
    appearsIn
    friends {
      name
    }
  }"
   "type TripleList {
      numbers: [[[Int!]]]!
   }"

   "mutation{
  createHuman (name:$testname, friends:[]) {
    id
  }
}"])

(deftest test-schema
  (doseq [schema test-schemas]
    (testing (str "Test schema parsing and transforming. schema: " schema)
      (is (not (insta/failure? (parse schema)))))))

(def type-fields-kv-example
  "type Hello {
     world(flag: Boolean = true): String!
     this: Int
   }")

(def input-type-fields-kv-example
  "input Hello {
    world: String!
    this: Int
   }")

(def variable-kv-example
  "query WithDefaultValues(
     $a: Int = 1,
     $b: String! = \"ok\",
     $c: ComplexInput = { requiredField: true, intField: 3 }) {
       dog { name }
     }")

(deftest kv-pairs
  (testing "we can convert type-fields to a map"
    (is (= (-> (parse type-fields-kv-example) :type-system-definitions first :fields)
           {"world" {:arguments  {"flag" {:default-value true :type-name "Boolean"}}
                     :type-name "String" :required true}
            "this" {:type-name "Int"}})))
  (testing "we can convert input-type-fields to a map"
    (is (= (-> (parse input-type-fields-kv-example) :type-system-definitions first :fields)
           {"world" {:type-name "String" :required true}
            "this"  {:type-name "Int"}})))
  (testing "we can convert variables to a map"
    (is (= (-> (parse variable-kv-example) :operation-definitions first :variable-definitions)
           ;; FIXME: fix boolean and enum-value issue in transformer
           ;; {"a" {:default-value 1 :type-name "Int"}
           ;;   "b" {:default-value "ok" :type-name "String" :required true}
           ;;  "c" {:default-value {"requiredField" true "intField" 3} :type-name "ComplexInput"}}
           {"a" {:default-value 1 :type-name "Int"}
            "b" {:default-value "ok" :type-name "String" :required true}
            "c" {:default-value {"requiredField" [:enum-value [:name "true"]] "intField" 3} :type-name "ComplexInput"}}
           
           ))))
