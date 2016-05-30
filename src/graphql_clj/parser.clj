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
                (into [:selection] args))
   :Field (fn field [& args]
            (println "Field: " args)
            [:field (into {} args)])
   :Arguments (fn arguments [& args]
                (println "Arguments: " args)
                [:arguments (into {} args)])
   :Argument (fn argument [& args]
               (println "Argument: " args)
               (into {} args))
   :IntValue (fn int-value [v]
               (println "IntValue: " v)
               (Integer/parseInt v))
   :FloatValue (fn float-value [v]
                 (println "FloatValue: " v)
                 (Double. v))
   :Name (fn name [v]
           (println "Name: " v)
           [:name v])})

(defn transformer
  [parse-tree]
  (insta/transform
   transformation-test
   parse-tree))

(defn get-selection-object-name
  [selection]
  {:post [(not (nil? %))]}
  (println selection)
  (:name (second (second selection))))

(defn get-selection-name
  [selection]
  (println "get-selection-name: " selection)
  (:name (second (second selection))))

(defn get-selection-type [selection]
  (first (second selection)))

(defn collect-selection [col selection]
  (println "collect-selection: " selection)
  (let [selection-type (get-selection-type selection)
        response-key (get-selection-name selection)]
    (case selection-type
      :field (conj col selection))))

(defn collect-fields
  "CollectFields(objectType, selectionSet, visitedFragments)"
  [object-type selection-set visited-fragments]
  (reduce collect-selection [] selection-set))

(defn type-system-lookup
  [object-type object-name]
  (case object-type
    :root (case object-name
            "user" {:type "user"
                    :resolve-fn (fn [object] {:id "test"})}
            (throw (ex-info (format "Unknown object name: %s." object-name) {})))
    (throw (ex-info (format "Unknown object type: %s." object-type) {}))))

(defn get-field-type-from-object-type
  "FIXME"
  [object-type field-selection]
  (let [object-name (get-selection-object-name field-selection)
        type-info (type-system-lookup object-type object-name)]
    (:type type-info)))

(defn resolve-field-on-object
  "FIXME"
  [object-type object field-entry]
  (let [object-name (get-selection-object-name field-entry)
        type-info (type-system-lookup object-type object-name)
        resolve-fn (:resolve-fn type-info)]
    (resolve-fn object)))

(defn get-field-entry [object-type object fields]
  (let [first-field-selection (first fields)
        response-key (get-selection-name first-field-selection)
        field-type (get-field-type-from-object-type object-type first-field-selection)
        field-entry first-field-selection]
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object object-type object field-entry)]
        (if (nil? resolved-object)
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          nil ; FIXME
          )))))

(defn evaluate-selection-set
  [object-type object selection-set visited-fragments]
  (let [fields (collect-fields object-type selection-set visited-fragments)
        _ (println "fields: " fields)
        resolved-fields (get-field-entry object-type object fields)]
    resolved-fields))

(defn execute-query [query]
  (let [selection-set (:selection-set query)
        visitied-fragments nil]
    (evaluate-selection-set :root nil selection-set visitied-fragments)))

(defn execute-definition
  [definition]
  (println "execute-definition: " definition)
  (let [operation (:operation-definition definition)
        operation-type (:operation-type operation)]
    (case operation-type
      "query" (execute-query operation)
      (throw (ex-info (format "Unhandled operation type: %s." operation-type))))))

(defn execute
  [document]
  (let [root (first document)
        definitions (rest document)]
    (if (not (= root :document))
      (throw (ex-info (format "Root(%s) is not a valid document" root) {}))
      {:data (into {} (map execute-definition definitions))})))

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
