(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(defn parse
  [stmt]
  (println stmt)
  (let [parser (insta/parser (io/resource "graphql.bnf")
                             ;; :output-format :enlive
                             )]
    (parser stmt)))

(def transformation
  {:Document (fn document [& args]
                (println "Document: " args)
                (into {} args))
    :Definition (fn definition [& args]
                   (println "Definition: " args)
                  [:definition (vec args)])
    :OperationType (fn operation-type [type]
                     [:operation-type type])
    :OperationDefinition (fn operation-definition [& args]
                           (println "OperationDefinition: " args)
                           (into {} args))
    :SelectionSet (fn selection-set [& args]
                    (println "SelectionSet: " args)
                    [:selection-set (vec args)])
    :Selection (fn selection [& args]
                 (println "Selection: " args)
                 [:selection (vec args)])
    :Field (fn field [& args]
             (println "Field: " args)
             (into {} args))
    :Arguments (fn arguments [& args]
                 (println "Arguments: " args)
                 {:arguments args})
    :Argument (fn argument [& args]
                (println "Argument: " args)
                (into {} args))
    :Name (fn name [name] [:name name])
    :FloatValue (fn float-value [v] (Double. v))
    :IntValue (fn int-value [v] (Integer/parseInt v))})

(def transformation-test
  {:OperationDefinition (fn operation-definition [& args]
                          (println "operation-definition: " args)
                          (println "start")
                          (println (map (fn [a] (println "new: ")
                                          (if (> (count a) 2)
                                            (println "1" (first a) "2" (second a) "3" (nth a 2) "4" (nth a 3))) (count a))
                                        args))
                          (let [definition (into {} args)]
                            (println "good")
                            [:operation-definition definition]))
   :OperationType (fn operation-type [type]
                    (println "operation-type: " type)
                    [:operation-type type])
   :Definition (fn definition [& args]
                 (println "definition: " args)
                 (into {} args))
   :Document (fn document [& args]
               (println "document: " args)
               (into [:document] args))
   :SelectionSet (fn selection-set [& args]
                   (println "SelectionSet: " args)
                   [:selection-set args])
   :Selection (fn selection [& args]
                (println "Selection: " args)
                (let [selection (into {} args)
                      props (dissoc selection :selection-set)
                      selection-set (:selection-set selection)]
                  (if selection-set
                    (into [:selection props] selection-set)
                    [:selection props])))
   :Field (fn field [& args]
            (println "Field: " args)
            (into {} args))
   :Arguments (fn arguments [& args]
                (println "Arguments: " args)
                [:arguments (into {} args)])
   :Argument (fn argument [& args]
               (println "Argument: " args)
               (into {} args))})

(defn transformer
  [parse-tree]
  (insta/transform
   transformation-test
   parse-tree))

(def execution-transform-map
  "Deprecated"
  {:document (fn document [& args]
               (println "document: " args)
               (let [v (into {} args)]
                 {:data v}))
   :operation-definition (fn operation-definition [& args]
                           (println "operation-definition: " args)
                           (let [props (first args)
                                 v (into {:parant :root} args)
                                 name (or (:OperationType v))]
                             [name v]))
   })



(defn execute
  [document]
  (insta/transform execution-transform-map document))

(def statements
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
