(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.type :as type]
            [graphql-clj.resolver :as resolver]
            [instaparse.core :as insta]))

(defn get-selection-arguments
  [selection]
  (let [arguments (get-in selection [:selection :field :arguments])]
    arguments))

(defn build-arguments
  [selection variables]
  (let [arguments (get-selection-arguments selection)]
    (->> arguments
         (map (fn update-argument [[k v]]
                (let [variable-name (keyword (get-in v [:variable :name]))]
                  (if (map? v)
                    [k (get variables variable-name)]
                    [k v])))
              )
         (into {}))))

(defn get-selection-name
  [selection]
  (assert (:selection selection) (format "Not a valid selection: %s." selection))
  (if-let [name (or (get-in selection [:selection :field :name])
                 (get-in selection [:selection :fragment-spread :fragment-name]))]
    name
    (throw (ex-info (format "Selection Name is null for selection: %s." selection)
                    {:selection selection}))))

(defn get-selection-type [selection]
  (assert (:selection selection) (format "Not a valid selection: %s." selection))
  (let [opts (:selection selection)]
    (cond
      (:field opts) :field
      (:fragment-spread opts) :fragment-spread
      :default (throw (ex-info (format "Selection Type is not handled for selection: %s." selection)
                               {:type (:field opts)
                                :opts opts})))))

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
        ;; (throw (ex-info "TODO: add suport for selection type :fragment-spread") {})
        :fragment-spread (into col (expand-fragment selection-name fragments))
        (throw (ex-info (format "selection type(%s) is not supported yet." selection-type)
                        {:selection-type selection-type
                         :selection selection}))))))

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
  [context schema resolver-fn parent-type-name parent-object field-entry variables]
  (let [field-name (get-selection-name field-entry)
        arguments (build-arguments field-entry variables)
        resolver (resolver-fn parent-type-name field-name)]
    (assert parent-type-name "Parent type name is NULL!")
    (assert field-name (format "Field name is empty for feild: %s." field-entry))
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
      (is-list-field-type? field-type) (map #(execute-fields context schema resolver-fn (type/get-inner-type schema field-type) % sub-selection-set fragments variables) result)
      (is-not-null-type? field-type) (let [not-null-result (complete-value context schema resolver-fn (type/get-inner-type schema field-type) result sub-selection-set fragments variables)]
                                       (if not-null-result
                                         not-null-result
                                         (throw (ex-info (format "NOT_NULL type %s returns null." field-type {:field-type field-type})))))
      :else (throw (ex-info (format "Unhandled field type %s." field-type) {:field-type field-type})))
    (throw (ex-info (format "result is NULL, while complete-value for field-type: %s" field-type) {}))))

(defn get-field-entry [context schema resolver-fn parent-type parent-object field-entry fragments variables]
  (assert field-entry (format "field-entry is NULL, for parent-type %s." parent-type))
  (assert parent-type (format "parent-type is NULL, for field-entry %s." field-entry))
  (let [response-key (get-selection-name field-entry)
        parent-type-name (:name parent-type)
        field-type (type/get-field-type schema parent-type-name response-key)]
    (assert response-key "response-key is NULL!")
    (if (not (nil? field-type))
      (let [resolved-object (resolve-field-on-object context schema resolver-fn parent-type-name parent-object field-entry variables)
            field-selection-set (get-field-selection-set field-entry)
            fields (collect-fields field-selection-set fragments)]
        (if (nil? resolved-object) ; when field is not-null field, resolved-object might be nil.
          [response-key nil] ; If resolvedObject is null, return
                             ; tuple(responseKey, null), indicating
                             ; that an entry exists in the result map
                             ; whose value is null.
          (let [response-value (complete-value context schema resolver-fn field-type resolved-object fields fragments variables)]
            [response-key response-value])))
      (throw (ex-info "field-type is NULL!" {})))))

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
  (let [type (get-in definition [:operation-type :type])]
    (case type
      "query" (execute-query context schema resolver-fn definition fragments variables)
      "mutation" (execute-mutation context schema resolver-fn definition fragments variables)
      (throw (ex-info (format "Unhandled operation root type: %s." definition) {})))))

(defn execute-document
  [context schema resolver-fn document variables]
  (let [operation-definitions (:operation-definitions document)
        fragments (:fragments document)]
    (cond
      (empty? operation-definitions) (throw (ex-info (format "Document is invalid (%s)." document) {}))
      :else {:data (into {} (map (fn [definition]
                                   (execute-definition context schema resolver-fn definition fragments variables))
                                 operation-definitions))})))

(defn execute
  ([context schema resolver-fn ^String statement variables]
   (let [parsed-document (parser/parse statement)
         schema-resolver-fn (resolver/create-resolver-fn schema resolver-fn)]
     (cond
       (insta/failure? schema) (throw (ex-info (format "Schema is invalid (%s)." schema) {}))
       (insta/failure? parsed-document) (throw (ex-info (format "Query statement is invalid (%s)." statement) {}))
       :else (execute-document context schema schema-resolver-fn parsed-document variables))))
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
  )
