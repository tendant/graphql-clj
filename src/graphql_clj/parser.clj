(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def whitespace (insta/parser "whitespace = #'\\s+'"))

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

(defn- to-type-system-definition [_ definition]
  (merge {:type :type-system-definition}
         definition))

(defn- parse-int    [_ v] (Integer/parseInt v))
(defn- parse-double [_ v] (Double. v))
(defn- parse-string [_ & args] (clojure.string/join (map second args)))
(defn- parse-bool   [_ v] (= "true" v))

(defn- to-kv-map [k & args]
  (let [{:keys [name type type-field-type] :as m} (into {} args)]
    (assert name "Name is NULL!")
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
  {{:f to-ident}                  #{:Definition :SchemaType :DirectiveName :ArgumentValue}
   {:f to-document}               #{:Document}
   {:f to-operation-definition}   #{:OperationDefinition}
   {:f to-map}                    #{:OperationType :Selection :Field :Arguments :Directive :FragmentSpread :InlineFragment :SchemaTypes :QueryType :MutationType :DirectiveOnName :EnumField :TypeImplements :TypeFieldVariable :TypeFieldArguments :TypeFieldType :TypeFields :InputTypeFields :VariableDefinitions}
   {:f to-val}                    #{:Name :Value :TypeCondition :Type :EnumValue :TypeFieldVariableDefault :TypeFieldArgumentDefault}
   {:f to-val :k :type}           #{:Query :Mutation}
   {:f to-val :k :enum-type}      #{:EnumTypeInt}
   {:f to-vec}                    #{:SelectionSet}
   {:f to-name-value-pair}        #{:ObjectField}
   {:f to-unwrapped-name}         #{:Argument}
   {:f to-kv-map}                 #{:TypeField :InputTypeField :TypeFieldArgument :VariableDefinition}
   {:f parse-int}                 #{:IntValue}
   {:f parse-double}              #{:FloatValue}
   {:f parse-string}              #{:StringValue}
   {:f parse-bool}                #{:BooleanValue}
   {:f to-fragment-definition}    #{:FragmentDefinition}
   {:f to-unwrapped-val}          #{:NamedType :FragmentName :FragmentType :DefaultValue :Alias :VariableName}
   {:f to-type-system-type}       #{:InterfaceDefinition :EnumDefinition :UnionDefinition :SchemaDefinition :InputDefinition :DirectiveDefinition :ScalarDefinition :TypeExtensionDefinition :TypeDefinition}
   {:f to-type-system-definition} #{:TypeSystemDefinition}
   {:f to-singular-and-plural}    #{:EnumFields :TypeFieldVariables}
   {:f add-required}              #{:TypeFieldTypeRequired :NonNullType}
   {:f transform-type-names}      #{:TypeNames :UnionTypeNames}
   {:f to-list}                   #{:ListTypeName}
   {:f args->map}                 #{:EnumType :ObjectValue}})

(defn- render-transformation-fns
  "Invert the map of functions to tree tags (so instaparse receives tree tags to functions)."
  [{:keys [f k]} v]
  (map #(vector % (partial f (or k (->kebab-case %)))) v))

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
