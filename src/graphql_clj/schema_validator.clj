(ns graphql-clj.schema-validator
  (:require [graphql-clj.parser :as parser]
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

;; Appends an error to the error vector.  The second argument is used
;; to add the location information to the error.  Returns errors with
;; a new error appended to it.
(defn- err [errors node fmt & args]
  (let [m (meta node)]
    (conj errors {:message (apply format fmt args)
                  :start {:line (:instaparse.gll/start-line m)
                          :column (:instaparse.gll/start-column m)
                          :index (:instaparse.gll/start-index m)}
                  :end {:line (:instaparse.gll/end-line m)
                        :column (:instaparse.gll/end-column m)
                        :index (:instaparse.gll/end-index m)}})))

(def ^:private member-map-hierarchy
  (-> (make-hierarchy)
      (derive :type-definition :typedef)
      (derive :interface-definition :typedef)
      (derive :input-definition :typedef)))

;; Builds a map of relevant members in the type/interface/union etc...
;; The return value is a vector of [errors typedef'] where typedef' is
;; the argument typedef with an additional map field added.  When the
;; declaration is not a type, typedef' is nil.
(defmulti build-member-map (fn [errors tdef] (:tag tdef)) :hierarchy #'member-map-hierarchy)

(defmethod build-member-map :typedef [errors tdef]
  (loop [errors errors tdef tdef fields (:fields tdef)]
    (if (empty? fields)
      [errors tdef]
      (let [[field & fields] fields name (:name field)]
        (if-let [firstdecl (get-in tdef [:field-map name])]
          (recur (err errors field "field '%s' already declared in '%s'" name (:name tdef)) tdef fields)
          (recur errors (assoc-in tdef [:field-map name] field) fields))))))

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
      

(defn- print-pass [x] x) ;; (pprint x) x)

(def ^:private introspection-type-map
  (-> (build-type-map [] {} (parser/parse-schema introspection-schema))
      (get-in [1 :type-map])))

(defn validate-schema
  [schema]
  (let [[errors schema] (print-pass (build-type-map [] introspection-type-map schema))
        errors (check-types-members errors schema)]
    [errors schema]))
