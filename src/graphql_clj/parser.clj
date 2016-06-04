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
                (let [props (into {} args)]
                  (println "Selection: " props)
                  [:selection props]))
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
  (println "get-selection-object-name: " selection)
  (let [name (get-in (second selection) [:field :name])]
    (println "get-selection-object-name: name: " name)
    name))

(defn get-selection-name
  [selection]
  (println "get-selection-name: " selection)
  (let [name (get-in (second selection) [:field :name])]
    (println "get-selection-name: name: " name)
    name))

(defn get-selection-type [selection]
  (println "get-selection-type: selection: " selection)
  (let [opts (second selection)]
    (cond
      (:field opts) :field
      :default (throw (ex-info (format "Selection Type is not handled for selection: %s." selection) {:type (:field opts)
                                                                                             :opts opts})))))

(defn get-field-selection-set [selection]
  (println "*** get-field-selection-set: selection: " selection)
  (let [opts (second selection)
        selection-set (get-in opts [:field :selection-set])]
    (println "field: " opts)
    (println "get-field-selection-set: selection-set: " selection-set)
    selection-set))

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

(comment ; Type kind
  :SCALAR
  :OBJECT
  :INTERFACE
  :UNION
  :ENUM
  :INPUT_OBJECT
  :LIST
  :NOT_NULL)

(def type-map
  {:GraphQLString {:name "String"
                   :kind :SCALAR}
   :UserType {:name "User"
              :kind :OBJECT
              :fields {:id {:type :GraphQLString}
                       :name {:type :GraphQLString}
                       :profilePic {:type :GraphQLString}}
              :args {}
              :resolve-fn (fn [object]
                            (println "user resolve-fn: ")
                            {:id "test"
                             :name "good"
                             :additional "extra"})}})

(def schema
  {:query {:name "Query"
           :kind :OBJECT
           :fields {:user {:type :UserType}}
           :resolve-fn (fn [obj]
                         (println "query resolve-fn:" obj)
                         (identity obj))}})

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
    (println "type: " type)
    (if (map? type)
      type
      (get type-map type))))

(defn get-field-type-from-object-type
  "FIXME"
  [object-type field-selection]
  (println (format "get-field-type-from-object-type: object-type: %s." object-type))
  (println (format "get-field-type-from-object-type: field-selection: %s." field-selection))
  (let [object-name (get-selection-object-name field-selection)
        _ (println "object-name: " object-name)
        _ (println "object: " (get-in object-type [:fields]))
        type (get-in object-type [:fields (keyword object-name) :type])]
    (if (map? type)
      type
      (get-type-meta type))))

(defn resolve-field-on-object
  "FIXME"
  [object-type object field-entry]
  (let [object-name (get-selection-object-name field-entry)
        resolve-fn (:resolve-fn object-type)]
    (println "resolve-filed-on-object: object-type: " object-type)
    (println "resolve-field-on-object: resolve-fn: " resolve-fn)
    (resolve-fn object)))

(defn merge-selection-sets
  [selections]
  (let [sets (reduce (fn [col selection]
                       (println (format "merge-selection-sets: col: %s, selection: %s." col selection))
                       (let [field (:field (second selection))]
                         (println "merge-selection-sets: field-selection-set: " field)
                         (if field
                           (into col field)
                           col)))
                     [] selections)]
    (println "merge-selection-sets: sets: " sets)
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

(defn complete-value
  [field-type result sub-selection-set]
  (println "*** complete-value: ")
  (println "field-type: " field-type)
  (println "result: " result)
  (println "sub-selection-set: " sub-selection-set)
  ;;; TODO
  ;; (if (and (not nullable?)
  ;;          (nil? resolved-object))
  ;;   (throw ""))
  ;; FIXME
  (if result
    (cond
      (is-scalar-field-type? field-type) result
      (is-enum-field-type? field-type) result
      (is-object-field-type? field-type) (execute-fields field-type result sub-selection-set)
      :else (throw (ex-info (format "Not a valid field type %s." field-type) {:field-type field-type})))))

(defn get-field-entry [parent-type parent-object field]
  (println "*** get-field-entry: " field)
  (let [first-field-selection field
        response-key (get-selection-name first-field-selection)
        field-type (get-field-type-from-object-type parent-type first-field-selection)
        field-entry first-field-selection]
    (println "field-type" field-type)
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object parent-type parent-object field-entry)
            field-selection-set (get-field-selection-set field)]
        (println "get-field-entry: field-selection-set: " field-selection-set)
        (if (nil? resolved-object)
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [;; sub-selection-set (merge-selection-sets field-selection-set)
                response-value (complete-value field-type resolved-object field-selection-set)]
            response-value)))
      (println "WARNING: field-type is nil!"))))

(defn evaluate-selection-set
  [object-type object selection-set visited-fragments]
  (let [fields (collect-fields object-type selection-set visited-fragments)
        _ (println "fields: " fields)
        resolved-fields (get-field-entry object-type object fields)]
    resolved-fields))

(defn execute-fields
  [parent-type root-value fields]
  (println "*** execute-fields")
  (println "execute-fields: parent-type: " parent-type)
  (println "execute-fields: root-value: " root-value)
  (println "execute-fields: fields: " fields)
  (into {} (map (fn [field]
                  (let [response-key (get-selection-name field)
                        field-type (get-field-type-from-object-type parent-type field)
                        ;; resolved-object (resolve-field-on-object field-type root-value field)
                        field-entry (get-field-entry parent-type root-value field)]
                    field-entry))
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
  (println "*** execute-definition: " definition)
  (let [operation (:operation-definition definition)
        operation-type (:operation-type operation)]
    (case operation-type
      "query" (execute-query operation)
      (throw (ex-info (format "Unhandled operation root type: %s." operation-type))))))

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
