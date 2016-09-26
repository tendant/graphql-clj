(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [instaparse.core :as insta]
            [clojure.set :as set]))

(defn get-selection-arguments
  [selection]
  (let [arguments (get-in selection [:selection :field :arguments])]
    arguments))

(defn- update-argument [variables [k v]]
  (cond (and k v)               [k v]
        (not k)                 (gerror/throw-error (format "Argument value is missing for argument (%s)." k))
        (contains? variables k) [k (get variables k)]
        :else                   (gerror/throw-error (format "Variable(%s) is missing from input variables." k))))

(defn build-arguments
  [selection variables]
  (let [arguments (get-selection-arguments selection)]
    ;; TODO: handle case when arguments are defined in field, but no argument provided.
    (->> arguments (map (partial update-argument variables)) (into {}))))

(defn get-selection-name
  [selection]
  (assert (:selection selection) (format "Not a valid selection: %s." selection))
  (if-let [name (or (get-in selection [:selection :field :name])
                 (get-in selection [:selection :fragment-spread :fragment-name]))]
    name
    (gerror/throw-error (format "Selection Name is null for selection: %s." selection))))

(defn get-selection-type [selection]
  (assert (:selection selection) (format "Not a valid selection: %s." selection))
  (let [opts (:selection selection)]
    (cond
      (:field opts) :field
      (:fragment-spread opts) :fragment-spread
      :default (gerror/throw-error (format "Selection Type is not handled for selection: %s." selection)))))

(defn get-field-selection-set [selection]
  (assert (:selection selection) (format "Not a valid selection: %s." selection))
  (let [opts (:selection selection)
        selection-set (get-in opts [:field :selection-set])]
    selection-set))

(defn expand-fragment [fragment-name fragments]
  (let [fragment (get fragments fragment-name)
        selection-set (:selection-set fragment)]
    (assert fragment (format "Cannot found fragment: %s." fragment-name))
    selection-set))

(defn collect-selection-fn
  [fragments]
  (fn [col selection]
    (assert (:selection selection) (format "Not a valid selection for collecting selection."))
    (let [selection-type (get-selection-type selection)
          selection-name (get-selection-name selection)]
      (case selection-type
        :field (conj col selection)
        :fragment-spread (into col (expand-fragment selection-name fragments))
        (gerror/throw-error (format "selection type(%s) is not supported yet." selection-type))))))

(defn collect-fields
  "CollectFields(objectType, selectionSet, visitedFragments)"
  [selection-set fragments]
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

(defn resolve-field-on-object
  [context schema resolver-fn parent-type parent-object field-entry field-type variables]
  (let [parent-type-name (:name parent-type)
        field-name (get-selection-name field-entry)
        field-arguments (type/get-field-arguments parent-type field-name)
        arguments (build-arguments field-entry variables)
        resolver (resolver-fn parent-type-name field-name)
        field-argument-keys (set (keys field-arguments))
        input-argument-keys (set (keys arguments))
        missing-arguments (set/difference field-argument-keys input-argument-keys)
        extra-arguments (set/difference input-argument-keys field-argument-keys)]
    (assert parent-type "Parent type is NULL!")
    (assert parent-type-name "Parent type name is NULL!")
    (assert field-name (format "Field name is empty for feild: %s." field-entry))
    ;; (assert (= (count missing-arguments) 0)
    ;;         (format "Missing arguments: %s." missing-arguments))
    (if (pos? (count missing-arguments))
      (gerror/throw-error (format "Missing arguments: %s, for field (%s) in type (%s)." missing-arguments field-name parent-type-name)))
    (if (pos? (count extra-arguments))
      (gerror/throw-error (format "Arguments(%s) are not defined for field (%s) in type (%s)." extra-arguments field-name parent-type-name)))
    (if (not (empty? arguments))
      (resolver context parent-object arguments)
      (resolver context parent-object))))

(defn is-enum-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :ENUM (:kind field-type-meta)))

(defn is-scalar-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :SCALAR (:kind field-type-meta)))

(defn is-object-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :OBJECT (:kind field-type-meta)))

(defn is-interface-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :INTERFACE (:kind field-type-meta)))

(defn is-union-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :UNION (:kind field-type-meta)))

(defn is-list-field-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :LIST (:kind field-type-meta)))

(defn is-not-null-type?
  [field-type-meta]
  (assert field-type-meta "field-type-meta is NULL!")
  (= :NOT_NULL (:kind field-type-meta)))

(declare execute-fields)

(defn complete-value
  [context schema resolver-fn field-type result sub-selection-set fragments variables]
  ;;; TODO
  ;; (if (and (not nullable?)
  ;;          (nil? resolved-object))
  ;;   (throw ""))
  ;; FIXME
  (assert field-type "field-type is NULL!")
  (if result
    (cond
      (is-scalar-field-type? field-type) result
      (is-enum-field-type? field-type) result
      (is-object-field-type? field-type) (execute-fields context schema resolver-fn field-type result sub-selection-set fragments variables)
      (is-interface-field-type? field-type) (execute-fields context schema resolver-fn field-type result sub-selection-set fragments variables)
      (is-list-field-type? field-type) (map #(complete-value context schema resolver-fn (type/get-inner-type schema field-type) % sub-selection-set fragments variables) result)
      (is-not-null-type? field-type) (let [not-null-result (complete-value context schema resolver-fn (type/get-inner-type schema field-type) result sub-selection-set fragments variables)]
                                       (if not-null-result
                                         not-null-result
                                         (gerror/throw-error (format "NOT_NULL type %s returns null." field-type))))
      :else (gerror/throw-error "Unhandled field type %s." field-type))
    (gerror/throw-error (format "result is NULL, while complete-value for field-type: %s" field-type))))

(defn get-field-entry [context schema resolver-fn parent-type parent-object field-entry fragments variables]
  (assert field-entry (format "field-entry is NULL, for parent-type %s." parent-type))
  (assert parent-type (format "parent-type is NULL, for field-entry %s." field-entry))
  (let [response-key (get-selection-name field-entry)
        parent-type-name (:name parent-type)
        field-type (type/get-field-type schema parent-type-name response-key)]
    (assert response-key "response-key is NULL!")
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object context schema resolver-fn parent-type parent-object field-entry field-type variables)
            field-selection-set (get-field-selection-set field-entry)
            fields (collect-fields field-selection-set fragments)]
        (if (nil? resolved-object) ; when field is not-null field, resolved-object might be nil.
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [response-value (complete-value context schema resolver-fn field-type resolved-object fields fragments variables)]
            [response-key response-value])))
      (gerror/throw-error (format "field-type is NULL for field(%s) in type(%s)!" response-key parent-type-name)))))

(defn execute-fields
  [context schema resolver-fn parent-type root-value fields fragments variables]
  (assert parent-type "parent-type is NULL!")
  (into {} (map (fn [field]
                  (let [response-key (get-selection-name field)
                        field-entry (get-field-entry context schema resolver-fn parent-type root-value field fragments variables)]
                    field-entry))
                fields)))

(defn execute-query [context schema resolver-fn query fragments variables]
  (assert query "query is NULL!")
  (assert (:selection-set query) "query selection-set is NULL!")
  (let [selection-set (:selection-set query)
        object-type (type/get-root-query-type schema)
        fields (collect-fields selection-set fragments)]
    (execute-fields context schema resolver-fn object-type :root fields fragments variables)))

(defn execute-mutation [context schema resolver-fn mutation fragments variables]
  (assert mutation "mutation is NULL!")
  (assert (:selection-set mutation) "mutation selection-set is NULL!")
  (let [selection-set (:selection-set mutation)
        object-type (type/get-root-mutation-type schema)
        fields (collect-fields selection-set fragments)]
    (execute-fields context schema resolver-fn object-type :root fields fragments variables)))

(defn execute-definition
  [context schema resolver-fn definition fragments variables]
  (assert definition "definition is NULL!")
  (if variables
    (assert (map? variables) "Input variables is not a map."))
  (let [type (get-in definition [:operation-type :type])
        operation-variable-keys (map :name (:variable-definitions definition))
        input-variable-keys (map (fn [[k _]] k) variables)
        missing-variables (set/difference (set operation-variable-keys)
                                          (set input-variable-keys))]
    (if (pos? (count missing-variables))
      (gerror/throw-error (format "Missing variable(%s) in input variables." missing-variables)))
    (case type
      "query" (execute-query context schema resolver-fn definition fragments variables)
      "mutation" (execute-mutation context schema resolver-fn definition fragments variables)
      (gerror/throw-error (format "Unhandled operation root type: %s." definition)))))

(defn execute-document
  [context schema resolver-fn document variables]
  (let [operation-definitions (:operation-definitions document)
        fragments (:fragments document)]
    (cond
      (empty? operation-definitions) (gerror/throw-error (format "Document is invalid (%s)." document))
      :else {:data (into {} (map (fn [definition]
                                   (execute-definition context schema resolver-fn definition fragments variables))
                                 operation-definitions))})))

(defn execute
  ([context schema resolver-fn ^String statement variables]
   (let [parsed-document (parser/parse statement)
         schema-resolver-fn (resolver/create-resolver-fn schema resolver-fn)]
     (cond
       (insta/failure? schema) (gerror/throw-error (format "Schema is invalid (%s)." schema))
       (insta/failure? parsed-document) {:error (parser/parse-error parsed-document)}
       :else (try
               (execute-document context schema schema-resolver-fn parsed-document variables)
               (catch Exception e
                 (if-let [error (ex-data e)]
                   {:error [error]}
                   (throw e)))))))
  ([context schema resolver-fn ^String statement]
   (execute context schema resolver-fn statement nil)))

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
  (let [type-schema (type/create-schema (parser/parse graphql-clj.introspection/introspection-schema))]
    (execute nil type-schema nil "query{__schema{types}}"))
  )
