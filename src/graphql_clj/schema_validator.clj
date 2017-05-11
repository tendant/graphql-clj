(ns graphql-clj.schema-validator
  (:require [graphql-clj.parser :as parser]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def ^:private introspection-schema
  "# From Section 4.2 Schema Introduction
scalar String
scalar Boolean
scalar Int
scalar Float

type __Schema {
  types: [__Type!]!
  queryType: __Type!
  mutationType: __Type
  directives: [__Directive!]!
}

type __Type {
  kind: __TypeKind!
  name: String
  description: String

  # OBJECT and INTERFACE only
  fields(includeDeprecated: Boolean = false): [__Field!]

  # OBJECT only
  interfaces: [__Type!]

  # INTERFACE and UNION only
  possibleTypes: [__Type!]

  # ENUM only
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

  # INPUT_OBJECT only
  inputFields: [__InputValue!]

  # NON_NULL and LIST only
  ofType: __Type
}

type __Field {
  name: String!
  description: String
  args: [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}

type __InputValue {
  name: String!
  description: String
  type: __Type!
  defaultValue: String
}

type __EnumValue {
  name: String!
  description: String
  isDeprecated: Boolean!
  deprecationReason: String
}

enum __TypeKind {
  SCALAR
  OBJECT
  INTERFACE
  UNION
  ENUM
  INPUT_OBJECT
  LIST
  NON_NULL
}

type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
}

enum __DirectiveLocation {
  QUERY
  MUTATION
  FIELD
  FRAGMENT_DEFINITION
  FRAGMENT_SPREAD
  INLINE_FRAGMENT
}")

(defn- reserved-name? [s]
  (str/starts-with? s "__"))

;; Appends an error to the error vector.  The second argument is used
;; to add the location information to the error.  Returns errors with
;; a new error appended to it.
(defn- err [errors node fmt & args]
  (let [m (meta node)]
    (conj errors {:message (apply format fmt args)
                  :start (:start m)
                  :end (:end m)})))

(def ^:private member-map-hierarchy
  (-> (make-hierarchy)
      (derive :type-definition :typedef)
      (derive :interface-definition :typedef)
      (derive :input-definition :typedef)))

(defn- build-argument-map [{:keys [arguments] :as field}]
  (if arguments
    (assoc field :arg-map (reduce #(assoc %1 (:name %2) %2) {} arguments))
    field))

;; Builds a map of relevant members in the type/interface/union etc...
;; The return value is a vector of [errors typedef'] where typedef' is
;; the argument typedef with an additional map field added.  When the
;; declaration is not a type, typedef' is nil.
(defmulti build-member-map (fn [errors tdef] (:tag tdef)) :hierarchy #'member-map-hierarchy)

(def default-typename-field {:tag :type-field
                             :name '__typename
                             :type {:tag :basic-type :name 'String :required true}})

(def default-fields [{:tag :type-field
                             :name '__typename
                             :type {:tag :basic-type :name 'String :required true}}])

(def default-field-map {'__typename default-typename-field})

(defmethod build-member-map :typedef [errors tdef]
  (let [init-fields (if (or (= :type-definition (:tag tdef))
                            (= :interface-definition (:tag tdef)))
                      default-fields
                      [])
        init-field-map (if (or (= :type-definition (:tag tdef))
                               (= :interface-definition (:tag tdef)))
                         default-field-map
                         {})]
    (loop [errors errors fields init-fields field-map init-field-map [f & fs :as fseq] (seq (:fields tdef))]
      (if (empty? fseq)
        [errors (assoc tdef :fields fields :field-map field-map)]
        (let [name (:name f)]
          (if-let [firstdecl (field-map name)]
            (recur (err errors f "field '%s' already declared in '%s'" name (:name tdef)) fields field-map fs)
            (let [f (build-argument-map f)]
              (recur errors (conj fields f) (assoc field-map name f) fs))))))))

(defmethod build-member-map :enum-definition [errors tdef]
  (loop [errors errors tdef tdef constants (:constants tdef)]
    (if (empty? constants)
      [errors tdef]
      (let [[c & constants] constants name (:name c)]
        (if-let [firstdecl (get-in tdef [:constant-map name])]
          (recur (err errors c "enum constant '%s' already declared in '%s'" name (:name tdef)) tdef constants)
          (recur errors (assoc-in tdef [:constant-map name] c) constants))))))

(defmethod build-member-map :union-definition [errors tdef]
  (loop [errors errors tdef tdef members (:members tdef)]
    (if (empty? members)
      [errors tdef]
      (let [[m & members] members n (:name m)]
        (if-let [firstdecl (get-in tdef [:member-map n])]
          (recur (err errors m "union member '%s' already declared in '%s'" n (:name tdef)) tdef members)
          (recur errors (assoc-in tdef [:member-map n] m) members))))))

(defmethod build-member-map :scalar-definition [errors tdef] [errors tdef])
(defmethod build-member-map :default [errors tdef] [errors nil])

;; build-type-map is the starting process of the validation.  It
;; builds maps for the fields/members/constants in definitions, and
;; builds a map from name to type declaration.  The returned schema
;; has additional :*-map fields.
(defn- build-type-map [errors tmap schema]
  (loop [tdefs (:type-system-definitions schema) tdefs' [] tmap tmap errors errors]
    (if-let [[tdef & tdefs] tdefs]
      (let [[errors tdef'] (build-member-map errors tdef)]
        (if (nil? tdef')
          (recur tdefs (conj tdefs' tdef) tmap errors)
          (let [pdef (tmap (:name tdef))]
            ;; We allow duplicate scalar definitions since they are
            ;; harmless and allow schemas to declare scalars that are
            ;; internally defined
            (if (or (nil? pdef) (and (= :scalar-definition (:tag pdef))
                                     (= :scalar-definition (:tag tdef))))
              (recur tdefs (conj tdefs' tdef') (assoc tmap (:name tdef) tdef') errors)
              (recur tdefs (conj tdefs' tdef') tmap (err errors tdef "type '%s' already declared" (:name tdef)))))))
      ;; replace the type-system-definition member with the updated
      ;; one, and add the type-map field.
      [errors (assoc schema :type-system-definitions tdefs' :type-map tmap)])))

(defn- raw-type-name [t]
  (if (= :list-type (:tag t))
    (recur (:inner-type t))
    (:name t)))

(defn- check-type-fields [errors tmap tdef]
  (loop [fields (:fields tdef) errors errors]
    (if-let [[f & fields] fields]
      (let [tname (raw-type-name (:type f))]
        (if (contains? tmap tname)
          (recur fields errors)
          (recur fields (err errors (:type f) "type '%s' referenced by field '%s' is not declared" tname (:name f)))))
      errors)))

(defn- check-union-members [errors tmap tdef]
  (loop [members (:members tdef) errors errors]
    (if-let [[m & members] members]
      (let [mname (:name m)]
        (if-let [mdef (tmap mname)]
          (if (= :type-definition (:tag mdef))
            (recur members errors)
            (recur members (err errors m "union member '%s' is not an object type" mname)))
          (recur members (err errors m "union member '%s' is not declared" mname))))
      errors)))

(defn- check-types-members [errors schema]
  (loop [tdefs (:type-system-definitions schema) errors errors]
    (if-let [[tdef & tdefs] tdefs]
      (case (:tag tdef)
        (:type-definition :interface-definition :input-definition)
        (recur tdefs (check-type-fields errors (:type-map schema) tdef))
        :union-definition
        (recur tdefs (check-union-members errors (:type-map schema) tdef))
        (recur tdefs errors))
      errors)))

(defn- validate-schema-roots [errors schema tdef]
  (loop [roots {} members (seq (:members tdef)) errors errors]
    (if-let [[m & members] members]
      (let [tag (:tag m) nm (:name m)]
        (if (contains? roots tag)
          (recur roots members (err errors m "'%s' root is already declared" (name tag)))
          (if (reserved-name? nm)
            (recur (assoc roots tag nil) members (err errors m "'%s' root type cannot use a reserved type '%s'" (name tag) nm))
            (if-let [tdef (get-in schema [:type-map nm])]
              (if (= :type-definition (:tag tdef))
                (recur (assoc roots tag nm) members errors)
                (recur (assoc roots tag nil) members (err errors m "'%s' root type '%s' must be an object type" (name tag) nm)))
              (recur (assoc roots tag nil) members (err errors m "'%s' root type '%s' is not declared" (name tag) nm))))))
      (if (contains? roots :query)
        [errors roots]
        [(err errors tdef "schema must declare a query root type") (assoc roots :query nil)]))))

(defn- check-schema-decl [errors schema]
  (loop [tdefs (:type-system-definitions schema) roots nil errors errors]
    (if-let [[tdef & tdefs] tdefs]
      (if (= :schema-definition (:tag tdef))
        (if roots
          (recur tdefs roots (err errors tdef "schema is already declared"))
          (let [[errors roots] (validate-schema-roots errors schema tdef)]
            (recur tdefs roots errors)))
        (recur tdefs roots errors))
      (if roots
        [errors (assoc schema :roots roots)]
        (if-let [tdef (get-in schema [:type-map 'QueryRoot])]
          (if (= :type-definition (:tag tdef))
            [errors (assoc schema :roots {:query 'QueryRoot})]
            [(err errors tdef "'query' root type 'QueryRoot' must be an object type") schema])
          [(err errors schema "schema does not define a query root or declare 'QueryRoot' type") schema])))))

(defn- print-pass [x] x) ;; (pprint x) x)

(def ^:private introspection-type-map
  (-> (build-type-map [] {} (parser/parse-schema introspection-schema))
      (get-in [1 :type-map])))

(defn update-root-query-with-introspection
  [query-root]
  (if query-root
    (let [schema-field {:tag :type-field
                        :name '__schema
                        :type {:tag :basic-type :name '__Schema :required true}
                        :kind :OBJECT}
          type-field {:tag :type-field
                      :name '__type
                      :type {:tag :basic-type :name '__Type}
                      :kind :OBJECT
                      :arguments [{:tag :argument-definition
                                   :name 'name
                                   :type {:tag :basic-type :name 'String :required true}}]
                      :arg-map {'name {:tag :argument-definition
                                       :name 'name
                                       :type {:tag :basic-type :name 'String :required true}}}}]
      (-> query-root
          (update :fields conj schema-field)
          (update :field-map assoc '__schema schema-field)
          (update :fields conj type-field)
          (update :field-map assoc '__type type-field)))
    query-root))

(defn- update-schema-with-introspection
  [schema]
  (let [query-root (get-in schema [:roots :query])]
    (update-in schema [:type-map query-root] update-root-query-with-introspection)))

(defn- validate-schema*
  [schema]
  ;; after validation add
  ;; {:type-system-definitions [ ... ]
  ;;  :type-map {'Dog {...} ... }
  ;;  :schema { ;; must have :query if present.  if not present, default is 'QueryRoot
  ;;     :query        'QueryRoot ;; not resolved, but validated that it exists, and that it is a 'type'
  ;;     :mutation     nil        ;; if not present = fine.  if present, check that type exists 'type'
  ;;     :subscription nil        ;;   ditto
  ;;  }
  (let [[errors schema] (print-pass (build-type-map [] introspection-type-map schema))
        errors (check-types-members errors schema)
        [errors schema] (check-schema-decl errors schema)
        schema-with-updated-root-query (update-schema-with-introspection schema)]
    (if (empty? errors)
      schema-with-updated-root-query
      (throw (ex-info "schema validation failed"
                      {:errors errors})))))

(defn validate-schema
  [schema]
  (let [parsed-schema (if (string? schema)
                           (parser/parse-schema schema)
                           schema)]
    (validate-schema* parsed-schema)))
