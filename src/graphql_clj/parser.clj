(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.set :as set]
            [graphql-clj.box :as box]))

(def graphql-bnf "graphql.bnf")

(def ^:private parser-fn (insta/parser (io/resource graphql-bnf)))

(defn- parse-statement
  "Parse graphql statement, hiccup format syntax tree will be returned for a valid graphql statement.
   An instance of instaparse.gll.Failure will be returned for parsing error."
  [stmt]
  (parser-fn stmt))

(defn- to-boxed-value [_ value] (box/kv->box value))
(defn- to-default-value [k arg] (to-boxed-value k (set/rename-keys arg {:value :default-value})))
(defn- args->map [_ & args] (into {} args))
(defn- to-ident [_ v] v)
(defn- to-map [k & args] {k (into {} args)})
(defn- to-vec [k & args] [k (vec args)])
(defn- to-val [k v] {k v})
(defn- to-type [_ v] (to-val :type v))
(defn- to-enum-type [_ v] (to-val :enum-type v))
(defn- parse-int    [_ v] (Integer/parseInt v))
(defn- parse-double [_ v] (Double. v))
(defn- parse-string [_ & args] (clojure.string/join (map second args)))
(defn- parse-bool   [_ v] (= "true" v))
(defn- unwrap-name [k v] {k (box/->Box (:name v) (meta v))})
(defn- to-one-or-more [_ & args] {:values (mapv :value args)})
(defn- to-type-names [_ & args] {:type-names (mapv :type-name args)})
(defn- to-list [_ arg] {:node-type :list :inner-type arg :kind :LIST})
(defn- add-required [_ arg] (assoc arg :required true))
(defn- to-document [_ & args] (group-by :section args))

(defn- to-type-system-type [k & args]
  (-> (into {:node-type k} args)
      (set/rename-keys {:type-field-arguments        :arguments
                        :type-field-argument-default :default-value})))

(defn- to-operation-definition [_ & args]
  (merge {:section        :operation-definitions
          :node-type      :operation-definition
          :operation-type {:type "query"}}                  ; default operation as query
         (into {} args)))

(defn- to-fragment-definition [k & args]
  (let [{:keys [name] :as definition} (into {} args)]
    (assert name "fragment name is NULL for fragment-definition!")
    (assoc definition :node-type k :section :fragment-definitions)))

(def node-type->kind
  {:type-definition      :OBJECT
   :input-definition     :INPUT_OBJECT
   :union-definition     :UNION
   :enum-definition      :ENUM
   :interface-definition :INTERFACE
   :directive-definition :DIRECTIVE
   :schema-definition    :SCHEMA})

(defn- to-type-system-definition [_ definition]
  (-> (assoc definition :section :type-system-definitions)
      (set/rename-keys {:type-fields       :fields
                        :enum-fields       :fields
                        :input-type-fields :fields
                        :directive-on-name :on})
      (assoc :kind (get node-type->kind (:node-type definition)))))

(def ^:private transformations
  "Map from transformation functions to applicable tree tags.
   This map gets rendered into the format expected by instaparse, e.g.: {:TreeTag fn}"
  {to-ident                  #{:Definition :SchemaType :DirectiveName :ListValue :TypeFieldType :Type :Selection :EnumValue}
   to-boxed-value            #{:ArgumentValue}
   to-document               #{:Document}
   to-operation-definition   #{:OperationDefinition}
   to-map                    #{:OperationType :QueryType :MutationType :DirectiveOnName :Implements}
   to-val                    #{:Name :Value :TypeCondition :TypeFieldArgumentDefault}
   unwrap-name               #{:TypeName :ArgumentName :FieldName :VariableName}
   to-type                   #{:Query :Mutation}
   to-enum-type              #{:EnumTypeInt}
   to-vec                    #{:SelectionSet :TypeFields :InputTypeFields :TypeFieldArguments :VariableDefinitions :Arguments :Directives :EnumFields :ObjectValue}
   parse-int                 #{:IntValue}
   parse-double              #{:FloatValue}
   parse-string              #{:StringValue}
   parse-bool                #{:BooleanValue}
   to-fragment-definition    #{:FragmentDefinition}
   to-type-system-type       #{:InterfaceDefinition :EnumDefinition :UnionDefinition :SchemaDefinition :InputDefinition :DirectiveDefinition :ScalarDefinition :TypeExtensionDefinition :TypeDefinition :TypeField :InputTypeField :TypeFieldArgument :Argument :Directive :EnumField :Field :FragmentSpread :InlineFragment :VariableDefinition}
   to-type-system-definition #{:TypeSystemDefinition}
   add-required              #{:TypeFieldTypeRequired :NonNullType}
   to-type-names             #{:TypeNames :UnionTypeNames}
   to-one-or-more            #{:OneOrMoreValue}
   to-list                   #{:ListTypeName :ListType}
   args->map                 #{:EnumType :ObjectField :FragmentName :FragmentType :Alias :SchemaTypes}
   to-default-value          #{:DefaultValue}})

(defn- render-transformation-fns
  "Invert the map of functions to tree tags (so instaparse receives tree tags to functions)."
  [f v]
  (map #(vector % (partial f (->kebab-case %))) v))

(def ^:private transformation-map
  "Map of tree tags to transformation functions that operate on the children."
  (->> transformations
       (mapcat (fn [[k v]] (render-transformation-fns k v)))
       (into {})))

(defn- transform
  "Transform parsed syntax tree for execution."
  [parsed-tree]
  (insta/transform transformation-map parsed-tree))

(defn parse
  "Parse graphql statment, parsed and transformed AST is returned if graphql statement is valid. An instance of instaparse.gll.Failure will be returned if graphql statement is invalid."
  [statement]
  (let [parsed-tree (insta/add-line-and-column-info-to-metadata statement (parse-statement statement))]
    (if (insta/failure? parsed-tree)
      parsed-tree
      (transform parsed-tree))))

(defn parse-error
  [e]
  (if (insta/failure? e)
    (let [{:keys [line column]} e]
      {:error  (format "Parse error at line %d, column %d." line column)
       :loc    {:line line :column column}})
    (throw (ex-info (format "Unhandled error: %s." e) {}))))
