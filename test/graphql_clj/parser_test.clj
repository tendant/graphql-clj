(ns graphql-clj.parser-test
  (:require [clojure.test :refer :all]
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
   
])

(deftest test-parse
  (testing "Test all statements parsing"
    (doseq [statement test-statements]
      (is (not (nil? (parse statement)))))))

(deftest test-transform
  (testing "Test statement transforming"
    (doseq [statement test-statements]
      (is (not (nil? (transform (parse statement))))))))

(def test-schemas
  ["
type Person {
  name: String
  age: Int
  picture: Url
}
"
   "
type Person {
  name(id: ID): String
  age: Int
  picture: Url
}"
   "
type Person {
  name(id: ID id2: ID): String
  age: Int
  picture: Url
}"
   "
type Person {
  name: String
  age: Int
}

type Photo {
  height: Int
  width: Int
}
"
   "
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
   "
type Person {
  name: String
  age: Int
  picture: Url
  relationship: Person
}"
   "
interface NamedEntity {
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
   "
union SearchResult = Photo | Person

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
   
   "
interface Being {
 name: String
}

interface Pet {
  name: String
}

interface Canine {
  name(surname: Boolean): String
}

enum DogCommand {
  SIT @enumInt(value: 0)
  HEEL @enumInt(value: 1)
  DOWN @enumInt(value: 2)
}

enum FurColor {
  BROWN @enumInt(value: 0)
  BLACK @enumInt(value: 1)
  TAN @enumInt(value: 2)
  SPOTTED @enumInt(value: 3)
}

type Dog implements Being, Pet, Canine {
  name(surname: Boolean): String
  nickname: String
  barks: Boolean
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand): Boolean
  isHousetrained(atOtherHomes: Boolean = true): Boolean
  isAtLocation(x: Int, y: Int): Boolean
}

type Cat implements Being, Pet {
  name: String
  nickname: String
  meows: Boolean
  meowVolume: Int
  furColor: FurColor
}

union CatOrDog = Dog | Cat

interface Intelligent {
  iq: Int
}

type Human implements Being, Intelligent {
  name(surname: Boolean): String
  pets: [Pet]
  relatives: [Human]
  iq: Int
}

type Alien implements Being, Intelligent {
  name(surname: Boolean): String
  iq: Int
  numEyes: Int
}

union DogOrHuman = Dog | Human

union HumanOrAlien = Human | Alien

input ComplexInput {
  requiredField: Boolean!
  intField: Int
  stringField: String
  booleanField: Boolean
  stringListField: [String]
}

type ComplicatedArgs {
  intArgField(intArg: Int): String
  nonNullIntArgField(nonNullIntArg: Int!): String
  stringArgField(stringArg: String): String
  booleanArgField(booleanArg: Boolean): String
  enumArgField(enumArg: FurColor): String
  floatArgField(floatArg: Float): String
  idArgField(idArg: ID): String
  stringListArgField(stringListArg: [String]): String
  complexArgField(complexArg: ComplexInput): String
  multipleReqs(req1: Int!, req2: Int!): String
  multipleOpts(opt1: Int, opt2: Int): String
  multipleOptAndReq(req1: Int!, req2: Int!, opt1: Int, opt2: Int): String
}

type QueryRoot {
  human(id: ID): Human
  alien: Alien
  dog: Dog
  cat: Cat
  pet: Pet
  catOrDog: CatOrDog
  dogOrHuman: DogOrHuman
  humanOrAlien: HumanOrAlien
  complicatedArgs: ComplicatedArgs
}"])

(deftest test-schema
  (testing "Test all schema parsing and transforming"
    (doseq [schema test-schemas]
      (is (not (nil? (parse schema))))
      (is (not (nil? (transform (parse schema))))))))
