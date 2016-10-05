(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def graphql-bnf "graphql.bnf")

(def ^:private parse- (insta/parser (io/resource graphql-bnf)))

(defn parse-debug
  [stmt]
  (insta/parse parse- stmt :partial true))

(defn- parse-statement
  "Parse graphql statement, hiccup format syntax tree will be returned for a valid graphql statement.
   An instance of instaparse.gll.Failure will be returned for parsing error."
  [stmt]
  (parse- stmt))

(defn- transform-type-names [_ & args]
  (let [names (into {} args)
        type-names (:type-names names)
        type-name (:name names)]
    [:type-names (conj type-names type-name)]))

(defn- args->map [_ & args] (into {} args))
(defn- to-ident [_ v] v)
(defn- to-map [k & args] {k (into {} args)})
(defn- to-vec [k & args] [k (vec args)])
(defn- to-val [k v] [k v])
(defn- to-type [_ v] (to-val :type v))
(defn- to-enum-type [_ v] (to-val :enum-type v))
(defn- to-type-system-type [k & args] (into {:type-system-type (keyword (str/replace (name k) #"-definition" ""))} args))
(defn- to-unwrapped-val [k v] [k (second v)])
(defn- to-list [_ & args]
  (let [{:keys [type-field-type]} (into {} args)]
    {:kind :LIST :inner-type (set/rename-keys type-field-type {:name :type-name})}))

(defn- add-required [_ arg]
  (cond
    (map? arg) (assoc arg :required true)
    (and (vector? arg) (= :named-type (first arg))) {:type-name (last arg) :required true}
    :else (throw (ex-info "unexpected to-required arg" arg))))

(defn- to-singular-and-plural [k & args]
  (let [base       (name k)
        singular-k (keyword (subs base 0 (- (count base) 1)))
        arg-map    (into {} args)]
    [k (conj (k arg-map) (singular-k arg-map))]))

(defn- to-document [_ & args]
  (let [operation-definitions (filter #(= :operation-definition (:type %)) args) ; FIXME: assume there is only one operation-definition
        type-definitions (filter #(= :type-system-definition (:type %)) args)
        fragment-definitions (filter #(= :fragment-definition (:type %)) args)
        fragments (reduce (fn reduce-fragments [v fragment]
                            (let [name (:fragment-name fragment)]
                              (assert name "fragment-name is NULL!")
                              (assoc v name fragment)))
                          {} fragment-definitions)]
    {:operation-definitions   operation-definitions
     :type-system-definitions type-definitions
     :fragments               fragments}))

(defn- to-operation-definition [_ & args]
  (merge {:type           :operation-definition
          :operation-type {:type "query"}}                  ; default operation as query
         (into {} args)))

(defn- to-name-value-pair [k [_ name] [_ value]]
  (assert name "Name is NULL!")
  [name value])

(defn- to-unwrapped-name [k [_ name] & value-args]
  (assert name "Name is NULL!")
  [name (into {} value-args)])

(defn- to-fragment-definition [_ & args]
  (let [{:keys [fragment-name] :as definition} (into {} args)]
    (assert fragment-name "fragment name is NULL for fragment-definition!")
    (assoc definition :type :fragment-definition)))

(def type-system-type->kind
  {:type      :OBJECT
   :input     :INPUT_OBJECT
   :union     :UNION
   :enum      :ENUM
   :interface :INTERFACE
   :directive :DIRECTIVE
   :schema    :SCHEMA})

(defn- to-type-system-definition [_ {:keys [type-system-type schema-types] :as definition}]
  (-> (dissoc definition :schema-types)
      (set/rename-keys {:name              :type-name
                        :type-fields       :fields
                        :enum-fields       :fields
                        :input-type-fields :fields
                        :directive-on-name :on})
      (merge schema-types)
      (assoc :type :type-system-definition
             :kind (get type-system-type->kind type-system-type))))

(defn- parse-int    [_ v] (Integer/parseInt v))
(defn- parse-double [_ v] (Double. v))
(defn- parse-string [_ & args] (clojure.string/join (map second args)))
(defn- parse-bool   [_ v] (= "true" v))

(defn- to-kv-map [k & args]
  (let [{:keys [name type type-field-type] :as m} (set/rename-keys (into {} args) {:variable-name :name})]
    (assert name (format "Name is NULL for (%s) (%s)!" k args))
    [name (-> m
              (dissoc :type-field-type :type :name)
              (merge type-field-type type)
              (set/rename-keys {:type-field-arguments        :arguments
                                :type-field-argument-default :default-value
                                :name                        :type-name
                                :named-type                  :type-name}))]))

(def ^:private transformations
  "Map from transformation functions to tree tags.
   This map gets rendered into the format expected by instaparse, e.g.: {:TreeTag fn}
   Use :k to specify non-standard names for the first argument to the relevant transformation function"
  {to-ident                  #{:Definition :SchemaType :DirectiveName :ArgumentValue :Type}
   to-document               #{:Document}
   to-operation-definition   #{:OperationDefinition}
   to-map                    #{:OperationType :Selection :Field :Arguments :Directive :FragmentSpread :InlineFragment :SchemaTypes :QueryType :MutationType :DirectiveOnName :EnumField :TypeImplements :TypeFieldVariable :TypeFieldArguments :TypeFieldType :TypeFields :InputTypeFields :VariableDefinitions}
   to-val                    #{:Name :Value :TypeCondition :EnumValue :TypeFieldVariableDefault :TypeFieldArgumentDefault}
   to-type                   #{:Query :Mutation}
   to-enum-type              #{:EnumTypeInt}
   to-vec                    #{:SelectionSet}
   to-name-value-pair        #{:ObjectField}
   to-unwrapped-name         #{:Argument}
   to-kv-map                 #{:TypeField :InputTypeField :TypeFieldArgument :VariableDefinition}
   parse-int                 #{:IntValue}
   parse-double              #{:FloatValue}
   parse-string              #{:StringValue}
   parse-bool                #{:BooleanValue}
   to-fragment-definition    #{:FragmentDefinition}
   to-unwrapped-val          #{:NamedType :FragmentName :FragmentType :DefaultValue :Alias :VariableName}
   to-type-system-type       #{:InterfaceDefinition :EnumDefinition :UnionDefinition :SchemaDefinition :InputDefinition :DirectiveDefinition :ScalarDefinition :TypeExtensionDefinition :TypeDefinition}
   to-type-system-definition #{:TypeSystemDefinition}
   to-singular-and-plural    #{:EnumFields :TypeFieldVariables}
   add-required              #{:TypeFieldTypeRequired :NonNullType}
   transform-type-names      #{:TypeNames :UnionTypeNames}
   to-list                   #{:ListTypeName}
   args->map                 #{:EnumType :ObjectValue}})

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
  (let [parsed-tree (parse-statement statement)]
    (if (insta/failure? parsed-tree)
      parsed-tree
      (transform parsed-tree))))

(defn parse-error
  [e]
  (if (insta/failure? e)
    (let [{:keys [line column]} e]
      {:message (format "Parse error at line %d, column %d." line column)
       :line line
       :column column})
    (throw (ex-info (format "Unhandled error: %s." e) {}))))
