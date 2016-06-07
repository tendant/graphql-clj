(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]))

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(defn parse
  [stmt]
  (log/debug stmt)
  (let [parser (insta/parser (io/resource "graphql.bnf")
                             ;; :output-format :enlive
                             )]
    (parser stmt)))

(def transformation-map
  {:OperationDefinition (fn operation-definition [& args]
                          (log/debug "operation-definition: " args)
                          (log/debug "start")
                          (log/debug (map (fn [a] (log/debug "new: ")
                                          (if (> (count a) 2)
                                            (log/debug "1" (first a) "2" (second a) "3" (nth a 2) "4" (nth a 3))) (count a))
                                        args))
                          (let [definition (into {:operation-type "query"} args)]
                            (log/debug "good")
                            [:operation-definition definition]))
   :OperationType (fn operation-type [type]
                    (log/debug "operation-type: " type)
                    [:operation-type type])
   :Definition (fn definition [& args]
                 (log/debug "definition: " args)
                 (into {} args))
   :Document (fn document [& args]
               (log/debug "document: " args)
               (into [:document] args))
   :SelectionSet (fn selection-set [& args]
                   (log/debug "SelectionSet: " args)
                   [:selection-set args])
   :Selection (fn selection [& args]
                (let [props (into {} args)]
                  (log/debug "Selection: " props)
                  [:selection props]))
   :Field (fn field [& args]
            (log/debug "Field: " args)
            [:field (into {} args)])
   :Arguments (fn arguments [& args]
                (log/debug "Arguments: " args)
                [:arguments (into {} args)])
   :Argument (fn argument [& args]
               (log/debug "Argument: " args)
               (into {} args))
   :IntValue (fn int-value [v]
               (log/debug "IntValue: " v)
               (Integer/parseInt v))
   :FloatValue (fn float-value [v]
                 (log/debug "FloatValue: " v)
                 (Double. v))
   :Name (fn name [v]
           (log/debug "Name: " v)
           [:name v])})

(defn transformer
  [parse-tree]
  (insta/transform
   transformation-map
   parse-tree))

(defn get-selection-object-name
  [selection]
  {:post [(not (nil? %))]}
  (log/debug "get-selection-object-name: " selection)
  (let [name (get-in (second selection) [:field :name])]
    (log/debug "get-selection-object-name: name: " name)
    name))

(defn get-selection-arguments
  [selection]
  (log/debug "get-selection-arguments: " selection)
  (let [arguments (get-in (second selection) [:field :arguments])]
    (log/debug "get-selection-arguments: arguments: " arguments)
    arguments))

(defn get-selection-name
  [selection]
  (log/debug "get-selection-name: " selection)
  (let [name (get-in (second selection) [:field :name])]
    (log/debug "get-selection-name: name: " name)
    name))

(defn get-selection-type [selection]
  (log/debug "get-selection-type: selection: " selection)
  (let [opts (second selection)]
    (cond
      (:field opts) :field
      :default (throw (ex-info (format "Selection Type is not handled for selection: %s." selection) {:type (:field opts)
                                                                                             :opts opts})))))

(defn get-field-selection-set [selection]
  (log/debug "*** get-field-selection-set: selection: " selection)
  (let [opts (second selection)
        selection-set (get-in opts [:field :selection-set])]
    (log/debug "field: " opts)
    (log/debug "get-field-selection-set: selection-set: " selection-set)
    selection-set))

(defn collect-selection [col selection]
  (log/debug "collect-selection: " selection)
  (let [selection-type (get-selection-type selection)
        response-key (get-selection-name selection)]
    (case selection-type
      :field (conj col selection))))

(defn collect-fields
  "CollectFields(objectType, selectionSet, visitedFragments)"
  [object-type selection-set visited-fragments]
  (reduce collect-selection [] selection-set))

(comment ; Type kind
  :SCALAR
  :OBJECT
  :INTERFACE
  :UNION
  :ENUM
  :INPUT_OBJECT
  :LIST
  :NOT_NULL)

(defn default-resolve-fn
  [field-name]
  (fn [& args]
    (let [parent (first args)]
      (get parent (keyword field-name)))))

(def type-map
  {:GraphQLString {:name "String"
                   :kind :SCALAR}
   :UserType {:name "User"
              :kind :OBJECT
              :fields {:id {:type :GraphQLString}
                       :name {:type :GraphQLString}
                       :profilePic {:type :ProfilePicType}}
              :args {}
              :resolve-fn (fn [& args]
                            (let [parent (first args)]
                              (log/debug "user resolve-fn: ")
                              (log/debug "user resolve-fn: parent: " parent)
                              {:id "test"
                               :name "good"
                               :additional "extra"}))}
   :ProfilePicType {:name "ProfilePic"
                    :kind :OBJECT
                    :fields {:resolution {:type :GraphQLString}
                             :url {:type :GraphQLString}}
                    :args {}
                    :resolve-fn (fn [& args]
                                  (let [parent (first args)
                                        arguments (second args)]
                                    (log/debug "profile pic resolve-fn.")
                                    (log/debug "profile pic parent: " parent)
                                    (log/debug "profile pic arguments: " arguments)
                                    {:resolution "480"
                                     :url "http://test.url.com"}))}})

(def schema
  {:query {:name "Query"
           :kind :OBJECT
           :fields {:user {:type :UserType}}
           :resolve-fn (fn [& args]
                         (let [parent (first args)]
                           (log/debug "query resolve-fn:" parent)
                           (identity parent)))}})

(defn get-type-meta
  [type-key]
  (or (get schema type-key)
      (get type-map type-key)
      (throw (ex-info (format "Unknown object type: %s." type-key)
                      {:type-key type-key}))))

(defn type-system-lookup
  [parent-object-type-key object-name]
  (let [parent-object-type (get-type-meta parent-object-type-key)
        type (or (get-in parent-object-type [:fields (keyword object-name) :type])
                 (throw (ex-info (format "Unknown object name: %s." parent-object-type-key)
                                 {:parent-object-type-key parent-object-type-key
                                  :object-name object-name})))]
    (log/debug "type: " type)
    (if (map? type)
      type
      (get-type-meta type))))

(defn get-field-type-from-object-type
  "FIXME"
  [object-type field-selection]
  (log/debug (format "get-field-type-from-object-type: object-type: %s." object-type))
  (log/debug (format "get-field-type-from-object-type: field-selection: %s." field-selection))
  (let [field-name (get-selection-object-name field-selection)
        _ (log/debug "field-name: " field-name)
        _ (log/debug "object: " (get-in object-type [:fields]))
        type (get-in object-type [:fields (keyword field-name) :type])]
    (cond
      (map? type) type
      (keyword? type) (get-type-meta type)
      (nil? type) (throw (ex-info (format "Cannot find field type (%s) in object(%s)." field-name object-type) {})))))

(defn resolve-field-on-object
  "FIXME"
  [parent-object field-type field-entry]
  (let [field-name (get-selection-object-name field-entry)
        arguments (get-selection-arguments field-entry)
        resolve-fn (or (:resolve-fn field-type)
                       (default-resolve-fn field-name))]
    (log/spy field-type)
    (log/spy field-entry)
    (log/spy resolve-fn)
    (log/spy parent-object)
    (log/spy arguments)
    (if arguments
      (log/spy (resolve-fn parent-object arguments))
      (log/spy (resolve-fn parent-object)))))

(defn merge-selection-sets
  [selections]
  (let [sets (reduce (fn [col selection]
                       (log/debug (format "merge-selection-sets: col: %s, selection: %s." col selection))
                       (let [field (:field (second selection))]
                         (log/debug "merge-selection-sets: field-selection-set: " field)
                         (if field
                           (into col field)
                           col)))
                     [] selections)]
    (log/debug "merge-selection-sets: sets: " sets)
    sets))

(defn is-enum-field-type?
  [field-type-meta]
  (= :ENUM (:kind field-type-meta)))

(defn is-scalar-field-type?
  [field-type-meta]
  (= :SCALAR (:kind field-type-meta)))

(defn is-object-field-type?
  [field-type-meta]
  (= :OBJECT (:kind field-type-meta)))

(defn is-interface-field-type?
  [field-type-meta]
  (= :INTERFACE (:kind field-type-meta)))

(defn is-union-field-type?
  [field-type-meta]
  (= :UNION (:kind field-type-meta)))

(declare execute-fields)

(defn complete-value
  [field-type result sub-selection-set]
  (log/debug "*** complete-value: ")
  (log/debug "field-type: " field-type)
  (log/debug "result: " result)
  (log/debug "sub-selection-set: " sub-selection-set)
  ;;; TODO
  ;; (if (and (not nullable?)
  ;;          (nil? resolved-object))
  ;;   (throw ""))
  ;; FIXME
  (if result
    (cond
      (is-scalar-field-type? field-type) result
      (is-enum-field-type? field-type) result
      (is-object-field-type? field-type) (log/spy (execute-fields field-type result sub-selection-set))
      :else (throw (ex-info (format "Unhandled field type %s." field-type) {:field-type field-type})))))

(defn get-field-entry [parent-type parent-object field]
  (log/debug "*** get-field-entry: " field)
  (let [first-field-selection field
        response-key (get-selection-name first-field-selection)
        field-type (get-field-type-from-object-type parent-type first-field-selection)
        field-entry first-field-selection]
    (log/debug "field-type" field-type)
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object parent-object field-type field-entry)
            field-selection-set (get-field-selection-set field)]
        (log/debug "get-field-entry: field-selection-set: " field-selection-set)
        (if (nil? resolved-object)
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [;; sub-selection-set (merge-selection-sets field-selection-set)
                response-value (complete-value field-type resolved-object field-selection-set)]
            [response-key (log/spy response-value)])))
      (log/debug "WARNING: field-type is nil!"))))

(defn evaluate-selection-set
  [object-type object selection-set visited-fragments]
  (let [fields (collect-fields object-type selection-set visited-fragments)
        _ (log/debug "fields: " fields)
        resolved-fields (get-field-entry object-type object fields)]
    resolved-fields))

(defn execute-fields
  [parent-type root-value fields]
  (log/debug "*** execute-fields")
  (log/debug "execute-fields: parent-type: " parent-type)
  (log/debug "execute-fields: root-value: " root-value)
  (log/debug "execute-fields: fields: " fields)
  (into {} (map (fn [field]
                  (let [response-key (get-selection-name field)
                        ;; field-type (get-field-type-from-object-type parent-type field)
                        ;; resolved-object (resolve-field-on-object field-type root-value field)
                        field-entry (get-field-entry parent-type root-value field)]
                    (log/spy field-entry)))
                fields)))

(defn execute-query [query]
  (let [selection-set (:selection-set query)
        visitied-fragments nil
        object-type (get-type-meta :query)
        fields (collect-fields object-type selection-set visitied-fragments)]
    ;; (evaluate-selection-set object-type :root selection-set visitied-fragments)
    (execute-fields object-type :root fields)))

(defn execute-definition
  [definition]
  (log/debug "*** execute-definition: " definition)
  (let [operation (:operation-definition definition)
        operation-type (:operation-type operation)]
    (case operation-type
      "query" (log/spy (execute-query operation))
      (throw (ex-info (format "Unhandled operation root type: %s." operation-type) {})))))

(defn execute
  [document]
  (let [root (first document)
        definitions (rest document)]
    (if (not (= root :document))
      (throw (ex-info (format "Root(%s) is not a valid document" root) {}))
      {:data (into {} (log/spy (map execute-definition definitions)))})))

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
