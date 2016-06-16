(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [graphql-clj.type :as type]))

(log/merge-config! {:level :info
                    :appenders {:println {:async? false}}})

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(def ^{:private true} parser- (insta/parser (io/resource "graphql.bnf")))

(defn parse
  [stmt]
  (log/debug stmt)
  (time (parser- stmt)))

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
               (let [m (into {} args)
                     name (:name m)
                     value (:value m)]
                 [name value]))
   :IntValue (fn int-value [v]
               (log/debug "IntValue: " v)
               (Integer/parseInt v))
   :FloatValue (fn float-value [v]
                 (log/debug "FloatValue: " v)
                 (Double. v))
   :Name (fn name [v]
           (log/debug "Name: " v)
           [:name v])
   :Value (fn value [v]
            (log/debug "Value: " v)
            [:value v])})

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
      :default (throw (ex-info (format "Selection Type is not handled for selection: %s." selection)
                               {:type (:field opts)
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

(defn get-field-type-from-object-type
  "FIXME"
  [type-meta-fn object-type field-selection]
  (log/debug (format "get-field-type-from-object-type: object-type: %s." object-type))
  (log/debug (format "get-field-type-from-object-type: field-selection: %s." field-selection))
  (let [field-name (get-selection-object-name field-selection)
        _ (log/debug "field-name: " field-name)
        _ (log/debug "object: " (get-in object-type [:fields]))
        type (get-in object-type [:fields (keyword field-name) :type])]
    (cond
      (map? type) type
      (keyword? type) (type-meta-fn type)
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

(defn is-list-field-type?
  [field-type-meta]
  (= :LIST (:kind field-type-meta)))

(defn get-field-inner-type
  [field-type-meta]
  (get field-type-meta ))

(declare execute-fields)

(defn complete-value
  [type-meta-fn field-type result sub-selection-set]
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
      (is-object-field-type? field-type) (log/spy (execute-fields type-meta-fn field-type result sub-selection-set))
      (is-list-field-type? field-type) (log/spy (map #(execute-fields type-meta-fn (type-meta-fn (:innerType field-type)) % sub-selection-set) result))
      :else (throw (ex-info (format "Unhandled field type %s." field-type) {:field-type field-type})))))

(defn get-field-entry [type-meta-fn parent-type parent-object field]
  (log/debug "*** get-field-entry: " field)
  (let [first-field-selection field
        response-key (get-selection-name first-field-selection)
        field-type (get-field-type-from-object-type type-meta-fn parent-type first-field-selection)
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
                response-value (complete-value type-meta-fn field-type resolved-object field-selection-set)]
            [response-key (log/spy response-value)])))
      (log/debug "WARNING: field-type is nil!"))))

(defn execute-fields
  [type-meta-fn parent-type root-value fields]
  (log/debug "*** execute-fields")
  (log/debug "execute-fields: parent-type: " parent-type)
  (log/debug "execute-fields: root-value: " root-value)
  (log/debug "execute-fields: fields: " fields)
  (into {} (map (fn [field]
                  (let [response-key (get-selection-name field)
                        ;; field-type (get-field-type-from-object-type parent-type field)
                        ;; resolved-object (resolve-field-on-object field-type root-value field)
                        field-entry (get-field-entry type-meta-fn parent-type root-value field)]
                    (log/spy field-entry)))
                fields)))

(defn execute-query [query type-meta-fn]
  (let [selection-set (:selection-set query)
        visitied-fragments nil
        object-type (type-meta-fn :query)
        fields (collect-fields object-type selection-set visitied-fragments)]
    (execute-fields type-meta-fn object-type :root fields)))

(defn execute-definition
  [definition type-meta-fn]
  (log/debug "*** execute-definition: " definition)
  (let [operation (:operation-definition definition)
        operation-type (:operation-type operation)]
    (case operation-type
      "query" (log/spy (execute-query operation type-meta-fn))
      (throw (ex-info (format "Unhandled operation root type: %s." operation-type) {})))))

(defn execute
  [document type-meta-fn]
  (let [root (first document)
        definitions (rest document)]
    (if (not (= root :document))
      (throw (ex-info (format "Root(%s) is not a valid document" root) {}))
      {:data (into {} (log/spy (map (fn [definition]
                                      (execute-definition definition type-meta-fn)) definitions)))})))

(comment
  ;; Sample expressions
  (parse "query {user}")
  (parse "query {user {id}}")
  (transformer (parse "query {user {id}}"))
  (execute (transformer (parse "query {user {id}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute (transformer (parse "query {user {id name}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute (transformer (parse "query {user {id name profilePic {url}}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute (transformer (parse "query {user {id name friends {name}}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute (transformer (parse "{
  __schema {
    types {
      name
    }
  }
}"))
           (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  )
