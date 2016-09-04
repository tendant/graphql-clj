(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [graphql-clj.resolver :as resolver]
            [instaparse.core :as insta]
            [taoensso.timbre :as log]))

(defn get-selection-arguments
  [selection]
  (log/debug "get-selection-arguments: " selection)
  (let [arguments (get-in (second selection) [:field :arguments])]
    (log/debug "get-selection-arguments: arguments: " arguments)
    arguments))

(defn get-selection-name
  [selection]
  (log/debug "get-selection-name: " selection)
  (let [name (or (get-in (second selection) [:field :name])
                 (get-in (second selection) [:fragment-spread :fragment-name]))]
    (log/debug "get-selection-name: name: " name)
    (if name
      name
      (throw (ex-info (format "Selection Name is null for selection: %s." selection)
                      {:selection selection})))))

(defn get-selection-type [selection]
  (log/debug "get-selection-type: selection: " selection)
  (let [opts (second selection)]
    (cond
      (:field opts) :field
      (:fragment-spread opts) :fragment-spread
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

(defn expand-fragment [fragment-name fragments]
  (log/debug "expand-fragment: " fragment-name)
  (log/debug "expand-fragment: fragments: " fragments)
  (let [fragment (get fragments fragment-name)
        selection-set (:selection-set fragment)]
    (if fragment
      selection-set
      (throw (ex-info (format "expand-fragment: cannot find fragment(%s)." fragment-name)
                      {:fragment-name fragment-name
                       :fragments fragments})))))

(defn collect-selection-fn
  [fragments]
  (fn [col selection]
    (log/debug "collect-selection-fn: " selection)
    (log/debug "collect-selection-fn: col: " col)
    (log/debug "collect-selection-fn: fragments: " fragments)
    (let [selection-type (get-selection-type selection)
          selection-name (get-selection-name selection)]
      (log/debug "selection-type: " selection-type)
      (case selection-type
        :field (conj col selection)
        ;; (throw (ex-info "TODO: add suport for selection type :fragment-spread") {})
        :fragment-spread (into col (expand-fragment selection-name fragments))
        (throw (ex-info (format "selection type(%s) is not supported yet." selection-type)
                        {:selection-type selection-type
                         :selection selection}))))))

(defn collect-fields
  "CollectFields(objectType, selectionSet, visitedFragments)"
  [selection-set fragments]
  (log/debug "collect-fields: selection-set" selection-set)
  (log/debug "collect-fields: fragments: " fragments)
  (reduce (collect-selection-fn fragments) [] selection-set))

(comment ; Type kind
  :SCALAR
  :OBJECT
  :INTERFACE
  :UNION
  :ENUM
  :INPUT_OBJECT
  :LIST
  :NOT_NULL)

;; (defn default-resolve-fn
;;   [field-name]
;;   (fn [& args]
;;     (let [context (first args)
;;           parent (second args)]
;;       (get parent (keyword field-name)))))

;; (defn get-field-type-from-object-type
;;   "FIXME"
;;   [schema resolver-fn object-type field-selection]
;;   (log/debug (format "get-field-type-from-object-type: object-type: %s." object-type))
;;   (log/debug (format "get-field-type-from-object-type: field-selection: %s." field-selection))
;;   (let [field-name (get-selection-name field-selection)
;;         _ (log/debug "field-name: " field-name)
;;         _ (log/debug "object: " (get-in object-type [:fields]))
;;         type (get-in object-type [:fields (keyword field-name) :type])]
;;     (cond
;;       (map? type) type
;;       (keyword? type) (type/get-type-in-schema type)
;;       (nil? type) (throw (ex-info (format "Cannot find field type (%s) in object(%s)." field-name object-type) {})))))

(defn resolve-field-on-object
  "FIXME"
  [context schema resolver-fn parent-type-name parent-object field-entry]
  (log/debug "*** resolve-field-on-object: ")
  (log/debug "resolve-field-on-object: parent-type-name: " parent-type-name)
  (log/debug "resolve-field-on-object: parent-object: " parent-object)
  (log/debug "resolve-field-on-object: field-entry: " field-entry)
  (let [field-name (get-selection-name field-entry)
        arguments (get-selection-arguments field-entry)
        resolve-fn (resolver-fn parent-type-name field-name)]
    (log/debug "resolve-field-on-object: arguments: " arguments)
    (if arguments
      (resolve-fn context parent-object arguments)
      (resolve-fn context parent-object))))

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

(defn is-not-null-type?
  [field-type-meta]
  (= :NOT_NULL (:kind field-type-meta)))

;; (defn get-field-inner-type
;;   [field-type-meta]
;;   (get field-type-meta ))

(declare execute-fields)

(defn complete-value
  [context schema resolver-fn field-type result sub-selection-set fragments]
  (log/debug "*** complete-value: context: " context)
  (log/debug "field-type: " field-type)
  (log/debug "result: " result)
  (log/debug "sub-selection-set: " sub-selection-set)
  (log/debug "complete-value: fragments: " fragments)
  ;;; TODO
  ;; (if (and (not nullable?)
  ;;          (nil? resolved-object))
  ;;   (throw ""))
  ;; FIXME
  (if result
    (cond
      (is-scalar-field-type? field-type) result
      (is-enum-field-type? field-type) result
      (is-object-field-type? field-type) (execute-fields context schema resolver-fn field-type result sub-selection-set fragments)
      (is-list-field-type? field-type) (map #(execute-fields context schema resolver-fn (type/get-inner-type schema field-type) % sub-selection-set fragments) result)
      (is-not-null-type? field-type) (let [not-null-result (complete-value context schema resolver-fn (type/get-inner-type schema field-type) result sub-selection-set fragments)]
                                       (if not-null-result
                                         not-null-result
                                         (throw (ex-info (format "NOT_NULL type %s returns null." field-type {:field-type field-type})))))
      :else (throw (ex-info (format "Unhandled field type %s." field-type) {:field-type field-type})))))

(defn get-field-entry [context schema resolver-fn parent-type parent-object field-entry fragments]
  (log/debug "*** get-field-entry: field-entry: " field-entry)
  (log/debug "get-field-entry: fragments: " fragments)
  (log/debug "get-field-entry: parent-object: " parent-object)
  (log/debug "get-field-entry: parent-type: " parent-type)
  (let [response-key (get-selection-name field-entry)
        ;; field-type (get-field-type-from-object-type schema resolver-fn parent-type first-field-selection)
        parent-type-name (:name parent-type)
        field-type (type/get-field-type schema parent-type-name response-key)]
    (log/debug "get-field-entry: field-type: " field-type)
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object context schema resolver-fn parent-type-name parent-object field-entry)
            field-selection-set (get-field-selection-set field-entry)
            fields (collect-fields field-selection-set fragments)]
        (log/debug "get-field-entry: fields: " fields)
        (log/debug "get-field-entry: resolved-object: " resolved-object)
        (if (nil? resolved-object) ; when field is not-null field, resolved-object might be nil.
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [;; sub-selection-set (merge-selection-sets field-selection-set)
                response-value (complete-value context schema resolver-fn field-type resolved-object fields fragments)]
            (log/debug "get-field-entry: response-value: " response-value)
            [response-key response-value])))
      (log/debug "WARNING: field-type is nil!"))))

(defn execute-fields
  [context schema resolver-fn parent-type root-value fields fragments]
  (log/debug "*** execute-fields")
  (log/debug "execute-fields: parent-type: " parent-type)
  (log/debug "execute-fields: root-value: " root-value)
  (log/debug "execute-fields: fields: " fields)
  (log/debug "execute-fields: fragments: " fragments)
  (into {} (map (fn [field]
                  (let [response-key (get-selection-name field)
                        ;; field-type (get-field-type-from-object-type parent-type field)
                        ;; resolved-object (resolve-field-on-object field-type root-value field)
                        field-entry (get-field-entry context schema resolver-fn parent-type root-value field fragments)]
                    field-entry))
                fields)))

(defn execute-query [context schema resolver-fn query fragments]
  (let [selection-set (:selection-set query)
        _ (log/debug "fragments: " fragments)
        object-type (type/get-root-query-type schema)
        fields (collect-fields selection-set fragments)]
    (execute-fields context schema resolver-fn object-type :root fields fragments)))

(defn execute-mutation [context schema resolver-fn mutation fragments]
  (let [selection-set (:selection-set mutation)
        object-type (type/get-root-mutation-type schema)
        fields (collect-fields selection-set fragments)]
    (execute-fields context schema resolver-fn object-type :root fields fragments)))

(defn execute-definition
  [context schema resolver-fn definition fragments]
  (log/debug "*** execute-definition: " definition)
  (let [type (get-in definition [:operation-type :type])]
    (log/debug "*** execute-definition: fragments: " fragments)
    (case type
      "query" (execute-query context schema resolver-fn definition fragments)
      "mutation" (execute-mutation context schema resolver-fn definition fragments)
      (throw (ex-info (format "Unhandled operation root type: %s." definition) {})))))

(defn execute-document
  [context schema resolver-fn document]
  (let [operation-definitions (:operation-definitions document)
        fragments (:fragments document)]
    (cond
      (empty? operation-definitions) (throw (ex-info (format "Document is invalid (%s)." document) {}))
      :else {:data (into {} (map (fn [definition]
                                   (execute-definition context schema resolver-fn definition fragments))
                                 operation-definitions))})))

(defn execute
  [context schema resolver-fn ^String statement]
  (let [parsed-document (parser/parse statement)
        schema-resolver-fn (resolver/create-resolver-fn schema resolver-fn)]
    (cond
      (insta/failure? schema) (throw (ex-info (format "Schema is invalid (%s)." schema) {}))
      (insta/failure? parsed-document) (throw (ex-info (format "Query statement is invalid (%s)." statement) {}))
      :else (execute-document context schema schema-resolver-fn parsed-document))))

(comment
  (execute nil (parser/transform (parser/parse "query {user {id}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute nil (parser/transform (parser/parse "query {user {id name}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute nil (parser/transform (parser/parse "query {user {id name profilePic {url}}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute nil (parser/transform (parser/parse "query {user {id name friends {name}}}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute nil (parser/transform (parser/parse "query {user { ...userFields friends {name}}} fragment userFields on UserType {id name}")) (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  (execute nil (parser/transform (parser/parse "{
  __schema {
    types {
      name
    }
  }
}"))
           (graphql-clj.type/create-type-meta-fn graphql-clj.type/demo-schema))
  )
