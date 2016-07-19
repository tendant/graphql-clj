(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [taoensso.timbre :as log]))

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

(defn expand-fragment [fragment-selection fragments]
  (log/debug "expand-fragment: " fragment-selection)
  (log/debug "expand-fragment: fragments: " fragments)
  (let [opts(second fragment-selection)
        fragment-name (get-in opts [:fragment-spread :fragment-name])
        fragment (get fragments fragment-name)
        selection-set (:selection-set fragment)]
    (if fragment
      (log/spy selection-set)
      (throw (ex-info (format "expand-fragment: cannot find fragment(%s)." fragment-selection)
                      {:fragment-selection fragment-selection
                       :fragments fragments})))))

(defn collect-selection-fn
  [fragments]
  (fn [col selection]
    (log/debug "collect-selection-fn: " selection)
    (log/debug "collect-selection-fn: col: " col)
    (log/debug "collect-selection-fn: fragments: " fragments)
    (let [selection-type (get-selection-type selection)
          response-key (get-selection-name selection)]
      (log/debug "selection-type: " selection-type)
      (case selection-type
        :field (conj col selection)
        ;; (throw (ex-info "TODO: add suport for selection type :fragment-spread") {})
        :fragment-spread (into col (expand-fragment selection fragments))
        (throw (ex-info (format "selection type(%s) is not supported yet." selection-type)
                        {:selection-type selection-type
                         :selection selection}))))))

(defn collect-fields
  "CollectFields(objectType, selectionSet, visitedFragments)"
  [object-type selection-set fragments]
  (log/debug "collect-fields: selection-set" selection-set)
  (log/debug "collect-fields: fragments: " fragments)
  (log/spy (reduce (collect-selection-fn fragments) [] selection-set)))

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
    (let [context (first args)
          parent (second args)]
      (get parent (keyword field-name)))))

(defn get-field-type-from-object-type
  "FIXME"
  [schema resolver-fn object-type field-selection]
  (log/debug (format "get-field-type-from-object-type: object-type: %s." object-type))
  (log/debug (format "get-field-type-from-object-type: field-selection: %s." field-selection))
  (let [field-name (get-selection-object-name field-selection)
        _ (log/debug "field-name: " field-name)
        _ (log/debug "object: " (get-in object-type [:fields]))
        type (get-in object-type [:fields (keyword field-name) :type])]
    (cond
      (map? type) type
      (keyword? type) (type/get-type-in-schema type)
      (nil? type) (throw (ex-info (format "Cannot find field type (%s) in object(%s)." field-name object-type) {})))))

(defn resolve-field-on-object
  "FIXME"
  [context schema resolver-fn parent-object field-type inner-field-type field-entry]
  (let [field-name (get-selection-object-name field-entry)
        arguments (get-selection-arguments field-entry)
        resolve-fn (or (:resolve-fn field-type)
                       (:resolve-fn inner-field-type)
                       (default-resolve-fn field-name))]
    (log/spy field-type)
    (log/spy field-entry)
    (log/spy resolve-fn)
    (log/spy parent-object)
    (log/spy arguments)
    (if arguments
      (log/spy (resolve-fn context parent-object arguments))
      (log/spy (resolve-fn context parent-object)))))

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
      (is-object-field-type? field-type) (log/spy (execute-fields context schema resolver-fn field-type result sub-selection-set fragments))
      (is-list-field-type? field-type) (log/spy (map #(execute-fields context schema resolver-fn (type/get-type-in-schema schema (:innerType field-type)) % sub-selection-set fragments) result))
      (is-not-null-type? field-type) (log/spy (let [not-null-result (complete-value context schema resolver-fn (type/get-type-in-schema schema (:innerType field-type)) result sub-selection-set fragments)]
                                                (if not-null-result
                                                  not-null-result
                                                  (throw (ex-info (format "NOT_NULL type %s returns null." field-type {:field-type field-type}))))))
      :else (throw (ex-info (format "Unhandled field type %s." field-type) {:field-type field-type})))))

(defn get-field-entry [context schema resolver-fn parent-type parent-object field fragments]
  (log/debug "*** get-field-entry: " field)
  (log/debug "get-field-entry: fragments: " fragments)
  (log/debug "get-field-entry: parent-object: " parent-object)
  (let [first-field-selection field
        response-key (get-selection-name first-field-selection)
        field-type (get-field-type-from-object-type schema resolver-fn parent-type first-field-selection)
        inner-type (:innerType field-type)
        inner-field-type (when inner-type (type/get-type-in-schema schema inner-type))
        field-entry first-field-selection]
    (log/debug "field-type" field-type)
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object context schema resolver-fn parent-object field-type inner-field-type field-entry)
            field-selection-set (get-field-selection-set field)
            fields (collect-fields field-type field-selection-set fragments)]
        (log/debug "get-field-entry: fields: " fields)
        (log/debug "get-field-entry: resolved-object: " resolved-object)
        (if (nil? resolved-object) ; when field is not-null field, resolved-object might be nil.
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [;; sub-selection-set (merge-selection-sets field-selection-set)
                response-value (complete-value context schema resolver-fn field-type resolved-object fields fragments)]
            [response-key (log/spy response-value)])))
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
                    (log/spy field-entry)))
                fields)))

(defn execute-query [context schema resolver-fn query fragments]
  (let [selection-set (:selection-set query)
        _ (log/debug "fragments: " fragments)
        object-type (type/get-type-in-schema schema :query)
        fields (collect-fields object-type selection-set fragments)]
    (execute-fields context schema resolver-fn object-type :root fields fragments)))

(defn execute-definition
  [context schema resolver-fn definition]
  (log/debug "*** execute-definition: " definition)
  (let [operation (:operation-definition definition)
        operation-type (:operation-type operation)
        fragments (:fragments definition)]
    (log/debug "*** execute-definition: fragments: " fragments)
    (case operation-type
      "query" (log/spy (execute-query context schema resolver-fn operation fragments))
      (throw (ex-info (format "Unhandled operation root type: %s." operation-type) {})))))

(defn execute
  [context schema resolver-fn document]
  (let [root (first document)
        definitions (rest document)]
    (if (not (= root :document))
      (throw (ex-info (format "Root(%s) is not a valid document" root) {}))
      {:data (into {} (log/spy (map (fn [definition]
                                      (execute-definition context schema resolver-fn definition)) definitions)))})))

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
