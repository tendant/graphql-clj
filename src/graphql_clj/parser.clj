(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [graphql-clj.type :as type]))

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(def graphql-bnf "graphql.bnf")

(def ^{:private true} parse- (insta/parser (io/resource graphql-bnf)))

(defn parse-debug
  [stmt]
  (insta/parse parse- stmt :partial true))

(defn- parse-statement
  "Parse graphql statement, hiccup format syntax tree will be return for a valid graphql statement. An instance of instaparse.gll.Failure will be return for parsing error."
  [stmt]
  (parse- stmt))

(def ^{:private true} transformation-map
  {;; Document
   :Definition (fn definition [definition]
                 definition)
   :Document (fn document [& args]
               (let [operation-definitions  (filter #(= :operation-definition (:type %)) args) ; FIXME: assume there is only one operation-definition
                     type-definitions (filter #(= :type-system-definition (:type %)) args)
                     fragment-definitions (filter #(= :fragment-definition (:type %)) args)
                     fragments (reduce (fn reduce-fragments [v fragment]
                                         (let [name (:fragment-name fragment)]
                                           (assert name "fragmet-name is NULL!")
                                           (assoc v name fragment)))
                                       {} fragment-definitions)]
                 {:operation-definitions operation-definitions
                  :type-system-definitions type-definitions
                  :fragments fragments}))

   ;; Begin Operation Definition
   :OperationDefinition (fn operation-definition [& args]
                          (let [definition (merge {:type :operation-definition
                                                   :operation-type {:type "query"}} ; default operation as query
                                                  (into {} args))]
                            definition))
   :OperationType (fn operation-type [& args]
                    [:operation-type (into {} args)])
   :Query (fn query [name]
            [:type name])
   :Mutation (fn mutation [name]
               [:type name])
   :SelectionSet (fn selection-set [& args]
                   [:selection-set args])
   :Selection (fn selection [& args]
                (let [props (into {} args)]
                  {:selection props}))
   :Field (fn field [& args]
            [:field (into {} args)])
   :Arguments (fn arguments [& args]
                [:arguments (into {} args)])
   :Argument (fn argument [& args]
               (let [m (into {} args)
                     name (:name m)
                     value (:value m)]
                 (assert name "Argument name is NULL!")
                 [name value]))
   :IntValue (fn int-value [v]
               (Integer/parseInt v))
   :FloatValue (fn float-value [v]
                 (Double. v))
   :StringValue (fn string-value [& args]
                  (clojure.string/join (map second args)))
   :Name (fn name [v]
           [:name v])
   :Value (fn value [v]
            [:value v])
   :FragmentDefinition (fn fragment-definition [& args]
                         (let [definition (into {} args)
                               fragment-name (:fragment-name definition)]
                           (assert fragment-name "fragment name is NULL for fragment-definition!")
                           (assoc definition :type :fragment-definition)))
   :TypeCondition (fn type-condition [v]
                    [:type-condition v])
   :NamedType (fn named-type [v]
                {:named-type (second v)})
   :FragmentName (fn fragment-name [v]
                   [:fragment-name (second v)])
   :Directive (fn directive [& args]
                [:directive (into {} args)])
   :FragmentSpread (fn fragment-spread [& args]
                     [:fragment-spread (into {} args)])
   :InlineFragment (fn inline-fragment [& args]
                     [:inline-fragment (into {} args)])

   ;; Begin Type System Definition
   :TypeSystemDefinition (fn type-system-definition [definition]
                           (merge {:type :type-system-definition}
                                  definition))
   
   :InterfaceDefinition (fn interface-definition [& args]
                          (into {:type-system-type :interface} args))
   :EnumDefinition (fn enum-definition [& args]
                     (into {:type-system-type :enum} args))
   :TypeDefinition (fn type-definition [& args]
                     (into {:type-system-type :type} args))
   :UnionDefinition (fn union-definition [& args]
                      (into {:type-system-type :union} args))
   :SchemaDefinition (fn schema-definition [& args]
                       (into {:type-system-type :schema}  args))
   :InputDefinition (fn input-definition [& args]
                      (into {:type-system-type :input} args))
   :DirectiveDefinition (fn directive-definition [& args]
                          (into {:type-system-type :directive} args))

   :SchemaTypes (fn schema-types [& args]
                  [:schema-types (into {} args)])
   :SchemaType (fn schema-type [arg]
                 arg)
   :QueryType (fn query-type [& args]
                [:query-type (into {} args)])
   :MutationType (fn mutation-type [& args]
                   [:mutation-type (into {} args)])
   :DirectiveName (fn directive-name [arg]
                    arg)
   :DirectiveOnName (fn directive-on-name [& args]
                      [:directive-on-name (into {} args)])
   :EnumFields (fn enum-fields [& args]
                 (let [fields (into {} args)
                       enum-fields (:enum-fields fields)
                       enum-field (:enum-field fields)]
                   [:enum-fields (conj enum-fields enum-field)]))
   :EnumField (fn enum-field [& args]
                [:enum-field (into {} args)])

   :TypeFields (fn type-fields [& args]
                 (let [fields (into {} args)
                       type-fields (:type-fields fields)
                       type-field (:type-field fields)]
                   [:type-fields (conj type-fields type-field)]))
   :TypeField (fn type-field [& args]
                [:type-field (into {} args)])

   :TypeFieldType (fn type-field-type [& args]
                    [:type-field-type (into {} args)])

   :TypeFieldTypeRequired (fn type-field-type-required [arg]
                            (update arg 1 merge {:required true}))

   :Type (fn type [arg]
           [:type arg])

   :TypeNames (fn type-names [& args]
                (let [names (into {} args)
                      type-names (:type-names names)
                      type-name (:name names)]
                  [:type-names (conj type-names type-name)]))

   :TypeFieldVariables (fn type-field-variables [& args]
                         (let [vars (into {} args)
                               variables (:type-field-variables vars)
                               variable (:type-field-variable vars)]
                           [:type-field-variables (conj variables variable)]))
   :TypeFieldVariable (fn type-field-variable [& args]
                        [:type-field-variable (into {} args)])

   :ListTypeName (fn list-type-name [& args]
                   {:kind :LIST
                    :innerType (into {} args)})
   :NonNullType (fn non-null-type [& args]
                  {:kind :NON_NULL
                   :innerType (into {} args)})

   :EnumType (fn enum-type [& args]
               (into {} args))
   :EnumTypeInt (fn enum-type [type-name]
                  [:enum-type type-name])
   :EnumValue (fn enum-value [value]
                [:enum-value value])

   :Variable (fn variable [& args]
               {:variable (into {} args)})
   })

(defn- transform
  "Transform parsed syntax tree for execution."
  [parsed-tree]
  (insta/transform
   transformation-map
   parsed-tree))

(defn parse
  [statement]
  (let [parsed-tree (parse-statement statement)]
    (if (insta/failure? parsed-tree)
      parsed-tree
      (transform parsed-tree))))

(comment
  ;; Sample expressions
  (parse "{user}")
  (parse "query {user}")
  (parse "query {user {id}}")
  (transform (parse "query {user {id}}"))
  (transform (parse "type Person {
  name: String
  age: Int
  picture: Url
}
"))
  (parse "mutation {createUser (email: \"user@test.com\") { id }}")
  (parse "{user (email: $email)}"))
