(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [graphql-clj.type :as type]))

(log/merge-config! {:level :debug
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
  {;; Document
   :Definition (fn definition [definition]
                 (log/debug "definition: " definition)
                 definition)
   :Document (fn document [& args]
               (log/debug "document: " args)
               (let [operation-definitions  (filter #(= :operation-definition (:type %)) args) ; FIXME: assume there is only one operation-definition
                     type-definitions (filter #(= :type-system-definition (:type %)) args)
                     fragment-definitions (filter #(= :fragment-definition (:type %)) args)
                     fragments (reduce (fn reduce-fragments [v fragment]
                                         (let [props (second fragment)
                                               name (:fragment-name props)]
                                           (assoc v name props)))
                                       {} fragment-definitions)]
                 {:operation-definitions operation-definitions
                  :type-system-definitions type-definitions
                  :fragments fragments}))

   ;; Begin Operation Definition
   :OperationDefinition (fn operation-definition [& args]
                          (log/debug "operation-definition: " args)
                          (let [definition (into {:type :operation-definition} args)]
                            (log/debug "operation-definition: definition" definition)
                            definition))
   :OperationType (fn operation-type [& args]
                    (log/debug "operation-type: args: " args)
                    [:operation-type (into {} args)])
   :Query (fn query [name]
            (log/debug "Query: " name)
            [:type name])
   :Mutation (fn mutation [name]
             (log/debug "Mutate: " name)
             [:type name])
   :SelectionSet (fn selection-set [& args]
                   (log/debug "SelectionSet: " args)
                   [:selection-set args])
   :Selection (fn selection [& args]
                (log/debug "Selection: " args)
                (let [props (into {} args)]
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
   :StringValue (fn string-value [& args]
                  (log/debug "StringValue: " args)
                  (clojure.string/join (map second args)))
   :Name (fn name [v]
           (log/debug "Name: " v)
           [:name v])
   :Value (fn value [v]
            (log/debug "Value: " v)
            [:value v])
   :FragmentDefinition (fn fragment-definition [& args]
                         (log/debug "FragmentDefinition: " args)
                         (let [definition (into {} args)
                               fragment-name (:fragment-name definition)]
                           {:type :fragment-definition
                            :definition definition}))
   :TypeCondition (fn type-condition [v]
                    (log/debug "TypeCondition: " v)
                    [:type-condition v])
   :NamedType (fn named-type [v]
                (log/debug "NamedType: " v)
                {:named-type (second v)})
   :FragmentName (fn fragment-name [v]
                   (log/debug "FragmentName: " v)
                   [:fragment-name (second v)])
   :Directive (fn directive [& args]
                (log/debug "Directive: " args)
                [:directive (into {} args)])
   :FragmentSpread (fn fragment-spread [& args]
                     (log/debug "FragmentSpread: " args)
                     [:fragment-spread (into {} args)])
   :InlineFragment (fn inline-fragment [& args]
                     (log/debug "InlineFragment: " args)
                     [:inline-fragment (into {} args)])

   ;; Begin Type System Definition
   :TypeSystemDefinition (fn type-system-definition [definition]
                           (log/debug "TypeSystemDefinition: " definition)
                           (merge {:type :type-system-definition}
                                  definition))
   
   :InterfaceDefinition (fn interface-definition [& args]
                          (log/debug "InterfaceDefinition: " args)
                          (into {:type-system-type :interface} args))
   :EnumDefinition (fn enum-definition [& args]
                     (log/debug "EnumDefinition: args: " args)
                     (into {:type-system-type :enum} args))
   :TypeDefinition (fn type-definition [& args]
                     (log/debug "TypeDefinition: args: " args)
                     (into {:type-system-type :type} args))
   :UnionDefinition (fn union-definition [& args]
                      (log/debug "UnionDefinition: args: " args)
                      (into {:type-system-type :union} args))
   :SchemaDefinition (fn schema-definition [& args]
                       (log/debug "SchemaDefinition: args: " args)
                       (into {:type-system-type :schema}  args))
   :InputDefinition (fn input-definition [& args]
                      (log/debug "InputDefinition: args: " args)
                      (into {:type-system-type :input} args))
   :DirectiveDefinition (fn directive-definition [& args]
                          (log/debug "DirectiveDefinition: args:" args)
                          (into {:type-system-type :directive} args))

   :DirectiveName (fn directive-name [arg]
                    (log/debug "DirectiveName: arg:" arg)
                    arg)
   :DirectiveOnName (fn directive-on-name [& args]
                      (log/debug "DirectiveOnName: args: " args)
                      [:directive-on-name (into {} args)])
   :EnumFields (fn enum-fields [& args]
                 (let [fields (into {} args)
                       enum-fields (:enum-fields fields)
                       enum-field (:enum-field fields)]
                   (log/debug "EnumFields: args:" args)
                   [:enum-fields (conj enum-fields enum-field)]))
   :EnumField (fn enum-field [& args]
                (log/debug "EnumField: args: " args)
                [:enum-field (into {} args)])

   :TypeFields (fn type-fields [& args]
                 (let [fields (into {} args)
                       type-fields (:type-fields fields)
                       type-field (:type-field fields)]
                   (log/debug "TypeFields: args: " args)
                   [:type-fields (conj type-fields type-field)]))
   :TypeField (fn type-field [& args]
                (log/debug "TypeField: args: " args)
                [:type-field (into {} args)])

   :TypeFieldType (fn type-field-type [& args]
                    (log/debug "TypeFieldType: args: " args)
                    [:type-field-type (into {} args)])

   :TypeFieldTypeRequired (fn type-field-type-required [arg]
                            (log/debug "TypeFieldTypeRequired: arg: " arg)
                            (update arg 1 merge {:required true}))

   :Type (fn type [arg]
           (log/debug "Type: arg: " arg)
           [:type arg])

   :TypeNames (fn type-names [& args]
                (let [names (into {} args)
                      type-names (:type-names names)
                      type-name (:name names)]
                  (log/debug "TypeNames: args: " args)
                  [:type-names (conj type-names type-name)]))

   :TypeFieldVariables (fn type-field-variables [& args]
                         (let [vars (into {} args)
                               variables (:type-field-variables vars)
                               variable (:type-field-variable vars)]
                           (log/debug "TypeFieldVariables: args: " args)
                           [:type-field-variables (conj variables variable)]))
   :TypeFieldVariable (fn type-field-variable [& args]
                        (log/debug "TypeFieldVariable: args: " args)
                        [:type-field-variable (into {} args)])

   :ListTypeName (fn list-type-name [& args]
                   (log/debug "ListTypeName: args" args)
                   {:kind :LIST
                    :innerType (into {} args)})
   :NonNullType (fn non-null-type [& args]
                  (log/debug "NonNullType: args" args)
                  {:kind :NON_NULL
                   :innerType (into {} args)})
   })

(defn transform
  [parse-tree]
  (insta/transform
   transformation-map
   parse-tree))

(comment
  ;; Sample expressions
  (parse "query {user}")
  (parse "query {user {id}}")
  (transform (parse "query {user {id}}"))
  (transform (parse "type Person {
  name: String
  age: Int
  picture: Url
}
")))
