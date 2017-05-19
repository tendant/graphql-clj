(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint] :as pp])
  (:import [graphql_clj Parser ParseException]))

(defn- unescape
  "Unescapes a string's escaped values according to the graphql spec."
  [str]
  (let [^java.util.regex.Matcher m (re-matcher #"\\(?:u([0-9A-Fa-f]{4})|.)" str)]
    (if-not (.find m)
      str
      (loop [buf (StringBuffer.)]
        ;; we could pass in the unescaped char as a replacement, but
        ;; then we have to convert it to string and quoteReplacement.
        ;; Its easier jsut to append "", the directly append the char
        ;; to the builder.
        (.appendReplacement m buf "")
        (->> (let [ch (.charAt str (inc (.start m)))]
               (case ch
                 \u (-> (.group m 1) (Integer/parseInt 16) char)
                 \n \newline
                 \r \return
                 \t \tab
                 \b \backspace
                 \f \formfeed
                 ch))
             (.append buf))
        (if (.find m)
          (recur buf)
          (.. m (appendTail buf) toString))))))

;; ^:meta {:fields [...]} =>
;; ^:meta [...]

(defn- mmove [ map ]
  (if (not= 1 (count map))
    map
    (let [[k v] (first map)]
      (assoc map k (with-meta v (meta map))))))

;; Like merge, but moves metadata from merged maps to contained items.
;; This allows location information to be transfered during merges.
(defn- mmerg [ map & maps ]
  (reduce #(conj %1 (mmove %2)) map maps))


;; TODO: compare having grammar with <ignored>? (with the '?') having
;; the ignored rule match the empty string and removing the '?' from
;; all rules.  (e.g., performance, validity, etc...)
(def ^:private rules
  {:type-system-definitions   ["<ignored>? ( type-system-definition <ignored>? )*"
                               (fn [ & defs ] {:tag :schema :type-system-definitions (vec defs)})]

   :type-system-definition    [" type-definition | interface-definition | union-definition
                               | schema-definition | enum-definition | input-definition
                               | directive-definition | extend-type-definition | scalar-definition"
                               identity]

   :type-definition           ["<'type'> <ignored> ident ( <ignored> implements )? <ignored>? type-fields"
                               (fn [ident & decls] (apply merge {:tag :type-definition :name ident} decls))]

   :implements                ["<'implements'> ( <ignored> basic-type-no-req )+" (fn [ & types ] {:implements types})]

   ;; remove the meta data to get the line number information from the
   ;; whole prodution instead of the sub-production.
   :extend-type-definition    ["<'extend'> <ignored> type-definition"
                               #(with-meta (assoc % :tag :extend-type-definition) nil)]

   :interface-definition      ["<'interface'> <ignored> ident <ignored>? type-fields"
                               #(merge {:tag :interface-definition :name %1} %2)]

   :input-definition          ["<'input'> <ignored> ident <ignored>? type-fields"
                               #(mmerg {:tag :input-definition :name %1} %2)]

   ;; members are coearsed to match the result of basic-type but
   ;; without the required.
   :union-definition          ["<'union'> <ignored> ident <ignored>? <'='> <ignored>? basic-type-no-req ( <ignored>? <'|'> <ignored>? basic-type-no-req )*"
                               (fn [ident & members]
                                 {:tag :union-definition :name ident :members (vec members)})]

   :schema-definition         ["<'schema'> <ignored>? <'{'> <ignored>? ( schema-type <ignored>? )* <'}'>"
                               (fn [ & members ] {:tag :schema-definition :members (vec members)})]

   :enum-definition           ["<'enum'> <ignored> ident <ignored>? <'{'> <ignored>? ( enum-constant <ignored>? )* <'}'>"
                               (fn [ ident & constants ] {:tag :enum-definition :name ident :constants (vec constants) })]

   :enum-constant             ["ident ( <ignored>? directives )?"
                               (fn enum-constant
                                 ([ident] {:tag :enum-constant :name ident})
                                 ([ident directives] (merge (enum-constant ident) directives)))]

   :directives                ["directive ( <ignored>? <directive> )*"
                               (fn [ & directives ] {:directives (vec directives)})]

   :directive                 ["<'@'> <ignored>? ident ( <ignored>? arguments )?"
                               (fn directive
                                 ([ident] {:tag :directive :name ident})
                                 ([ident arguments] (mmerg (directive ident) arguments)))]

   :directive-definition      ["<'directive'> <ignored> <'@'> <ignored>? ident ( <ignored> type-condition )?"
                               (fn
                                 ([ident] {:tag :directive-definition :name ident})
                                 ([ident type-condition] {:tag :directive-definition :name ident :on (:on type-condition)}))]

   :type-condition            ["<'on'> <ignored> basic-type-no-req" (fn [on] {:on on})]

   :scalar-definition         ["<'scalar'> <ignored> ident"
                               (fn[n] {:tag :scalar-definition :name n})]

   :type-fields               ["<'{'> <ignored>? ( type-field <ignored>? )* <'}'>" (fn [ & fields ] {:fields (vec fields)})]

   :type-field                ["ident <ignored>? ( arguments-definition <ignored>? )? <':'> <ignored>? type-ref"
                               (fn ([ident type] {:tag :type-field :name ident :type type})
                                  ([ident arguments type] {:tag :type-field :name ident :arguments arguments :type type}))]

   :arguments-definition      ["<'('> <ignored>? ( argument-definition <ignored>? )* <')'>" vector]

   :argument-definition       ["ident <ignored>? <':'> <ignored>? type-ref ( <ignored>? <'='> <ignored>? value )?"
                               (fn ([ident type] {:tag :argument-definition :name ident :type type})
                                  ([ident type value] {:tag :argument-definition :name ident :type type :default-value value}))]

   :arguments                 ["<'('> <ignored>? ( argument <ignored>? )* <')'>" (fn [ & args ] {:arguments (vec args)})]

   :argument                  ["ident <ignored>? <':'> <ignored>? value"
                               (fn[n v] {:tag :argument :name n :value v})]

   :type-ref                  ["list-type | basic-type" identity]

   :list-type                 ["<'['> <ignored>? type-ref <ignored>? <']'> ( <ignored>? required )?"
                               (fn list-type
                                 ([t] {:tag :list-type :inner-type t})
                                 ([t r] (assoc (list-type t) :required true)))]

   :basic-type-no-req         ["ident"
                               (fn [n] {:tag :basic-type :name n})]

   :basic-type                ["ident ( <ignored>? required )?"
                               (fn basic-type
                                 ([n] {:tag :basic-type :name n})
                                 ([n r] (assoc (basic-type n) :required true)))]

   :required                  ["<'!'>" (constantly true)]

   :schema-type               ["query-type | mutation-type | subscription-type" identity]
   
   :query-type                ["<'query'> <ignored>? <':'> <ignored>? ident"
                               (fn [n] {:tag :query :name n})]
   :mutation-type             ["<'mutation'> <ignored>? <':'> <ignored>? ident"
                               (fn [n] {:tag :mutation :name n})]
   :subscription-type         ["<'subscription'> <ignored>? <':'> <ignored>? ident"
                               (fn [n] {:tag :subscription :name n})]

   :value                     ["variable-reference | float-value | int-value | boolean-value | string-value | null-value | enum-value | list-value | object-value" identity]

   :variable-reference        ["<'$'> <ignored>? ident"
                               (fn [n] {:tag :variable-reference :name n})]

   :list-value                ["<'['> <ignored>? ( value <ignored>? )* <']'>"
                               (fn [ & values ] {:tag :list-value :values (vec values)})]

   ;; For object-values, avoid the temptation to place fields into a
   ;; map.  We need all fields for validation incase there are
   ;; duplicate fields.
   :object-value              ["<'{'> <ignored>? ( object-field <ignored>? )* <'}'>"
                               (fn [ & fields ] {:tag :object-value :fields (vec fields)})]

   :object-field              ["ident <ignored>? <':'> <ignored>? value"
                               (fn [ident value] {:tag :object-field :name ident :value value})]

   :float-value               ["#'-?(?:0|[1-9]\\d*)(?:\\.\\d+(?:[eE][-+]?\\d+)?|[eE][-+]?\\d+)'"
                               (fn [i] {:tag :float-value :image i :value (try (Double. i) (catch NumberFormatException ex))})]

   :int-value                 ["#'-?(?:0|[1-9]\\d*)(?![.eE])'" ;; use a negative lookahead to prevent matching the first part of a float
                               (fn [i] {:tag :int-value :image i :value (try (Long/parseLong i 10) (catch NumberFormatException ex))})]

   :boolean-value             ["true-value | false-value" identity]

   :enum-value                ["!(boolean-value | null-value) #'[_A-Za-z][_A-Za-z0-9]*'"
                               (fn [i] {:tag :enum-value :image i :value (symbol i)})]
   :true-value                ["'true'"  (fn [i] {:tag :boolean-value :image i :value true})]
   :false-value               ["'false'" (fn [i] {:tag :boolean-value :image i :value false})]
   :null-value                ["'null'"  (fn [i] {:tag :null-value    :image i :value nil})]
   :string-value              ["<'\"'> #'(?:[\\t\\u0020-\\uffff&&[^\"\\\\]]|\\\\(?:[trnfb/\"\\\\]|u[0-9A-Fa-f]{4}))*' <'\"'>"
                               (fn [i] {:tag :string-value :image i :value (unescape i)})]

   :ident                     ["#'[_A-Za-z][_A-Za-z0-9]*'" symbol]

   :ignored                   ["#'(?:[\\r\\n\\t\\u0020,\\ufeff]|#[\\t\\u0020-\\uffff]*)+'"
                               (constantly nil)]

   :query-document            ["<ignored>? ( ( query-definition | mutation-definition | fragment-definition | selection-set-operation ) <ignored>? )*" vector]

   :selection-set-operation   ["selection-set" #(assoc % :tag :selection-set)]

   :query-definition          ["<'query'> ( <ignored> ident )? <ignored>? operation-rest"
                               (fn ([oprest] (mmerg {:tag :query-definition} oprest))
                                 ([name oprest] (mmerg {:tag :query-definition :name name} oprest)))]
                                ;; (fn ([op] (assoc op :tag :query))
                                ;;   ([name op] (assoc op :tag :query :name name)))]

   :mutation-definition       ["<'mutation'> ( <ignored> ident )? <ignored>? operation-rest"
                                (fn ([op] (mmerg {:tag :mutation} op))
                                  ([name op] (mmerg {:tag :mutation :name name} op)))]

   :operation-rest            ["( variable-definitions <ignored>? )? ( directives <ignored>? )? selection-set"
                                (fn [ & parts ] (apply merge parts))]

   :fragment-definition       ["<'fragment'> <ignored> ident <ignored> ( type-condition <ignored>? )? ( directives <ignored>? )? selection-set"
                                (fn [name & rest] (apply mmerg {:tag :fragment-definition :name name} rest))]

   :variable-definitions      ["<'('> <ignored>? ( variable-definition <ignored>? )* <')'>"
                                (fn [ & defs ] {:variable-definitions (vec defs)})]

   :variable-definition       ["<'$'> <ignored>? ident <ignored>? <':'> <ignored>? type-ref ( <ignored>? <'='> <ignored>? value )?"
                                (fn ([name type] {:tag :variable-definition :name name :type type})
                                  ([name type value] {:tag :variable-definition :name name :type type :default-value value}))]

   :selection-set             ["<'{'> <ignored>? ( selection <ignored>? )* <'}'>"
                                (fn [ & selection ] {:selection-set (vec selection)})]

   :selection                 ["aliased-field | selection-field | fragment-spread | inline-fragment" identity]

   :aliased-field             ["ident <ignored>? <':'> <ignored>? selection-field"
                                (fn [ alias field ] (mmerg {:alias alias} field))]

   :selection-field           ["ident ( <ignored>? arguments )? ( <ignored>? directives )? ( <ignored>? selection-set )?"
                                (fn [ name & rest ] (apply mmerg {:tag :selection-field :name name} rest))]

   :fragment-spread           ["<'...'> <ignored>? ident ( <ignored>? directives )?"
                                (fn ([name] {:tag :fragment-spread :name name})
                                  ([name directives] (mmerg {:tag :fragment-spread :name name} directives)))]

   :inline-fragment           ["<'...'> <ignored>? ( type-condition <ignored>? )? ( directives <ignored> )? selection-set"
                                (fn [ & args ] (apply mmerg {:tag :inline-fragment} args))]
   })

(defn- production-refs [production]
  (for [[_ t] (re-seq #"'(?:[^']|\\.)*'|([a-zA-Z][-_a-zA-Z0-9]*)" production) :when t]
    (keyword t)))

(defn- collect-rules [rules-map rule]
  (loop [set #{rule} q (conj clojure.lang.PersistentQueue/EMPTY rule)]
    (if (empty? q) (into [rule] (disj set rule))
        (let [prods (into #{} (production-refs (first (rules-map (peek q)))))]
          (recur (into set prods) (into (pop q) (set/difference prods set)))))))

(defn- build-parser [rules-map primary]
  (let [rules (collect-rules rules-map primary)
        parser (insta/parser (str/join "\n" (map #(str (name %) " := " (first (rules-map %))) rules)))
        tfmap (reduce #(if-let [tf (second (rules-map %2))] (assoc %1 %2 tf) %1) {} rules)]
    #(->> (parser %)
          (insta/add-line-and-column-info-to-metadata %)
          (insta/transform tfmap))))

(def orig-parse-schema-fn (build-parser rules :type-system-definitions))
(def orig-parse-query-document-fn (build-parser rules :query-document))

(defn- insta-failure->error
  [message failure]
  {:message "Failed parsing schema!"
   :locations [{:line (:line failure)
                :column (:column failure)
                :index (:index failure)}]})

(defn orig-parse-schema
  [^String input]
  (let [result (orig-parse-schema-fn input)]
    (if (insta/failure? result)
      (throw (ex-info "Failed parsing schema." {:errors [(insta-failure->error "Failed parsing schema." result)]}))
      result)))

(defn orig-parse-query-document
  [^String input]
  (let [result (orig-parse-query-document-fn input)]
    (if (insta/failure? result)
      (throw (ex-info "Failed parsing query document." {:errors [(insta-failure->error "Failed parsing query document." result)]}))
      result)))

(defn- parse-exception->error
  [e]
  (if e
    (let [location (.location e)]
      {:message (.getMessage e)
       :locations [{:line (:line location)
                    :column (:column location)
                    :index (:index location)}]})))

(defn parse-schema [^String input]
  (try
    (.parseSchema (Parser. input))
    (catch ParseException e
      (throw (ex-info "Failed parse schema." {:errors [(parse-exception->error e)]})))))

(defn parse-query-document [^String input]
  (try
    (.parseQueryDocument (Parser. input))
    (catch ParseException e
      (throw (ex-info "Failed parse query document." {:errors [(parse-exception->error e)]})))))

(def ^:private example-schema
"enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type Arguments {
  multipleReqs(x: Int!, y: Int!): Int!
  booleanArgField(booleanArg: Boolean): Boolean
  floatArgField(floatArg: Float): Float
  intArgField(intArg: Int): Int
  nonNullBooleanArgField(nonNullBooleanArg: Boolean!): Boolean!
  nonNullListOfBooleanField(nonNullListOfBooleanArg: [Boolean]!) : Boolean!
  listOfNonNullBooleanField(listOfNonNullBooleanArg: [Boolean!]) : Boolean!
  nonNullListOfNonNullBooleanField(nonNullListOfNonNullBooleanArg: [Boolean!]!) : Boolean!
  booleanListArgField(booleanListArg: [Boolean]!): [Boolean]
}

type QueryRoot {
  dog: Dog
  arguments: Arguments
}

type MutationRoot {
  dog: Dog
  arguments: Arguments
}

schema {
  query: QueryRoot
  mutation: MutationRoot
}")


;; for benchmarking, performs a gc and waits for an object to be collected.
(defn- gc []
  (let [rq (java.lang.ref.ReferenceQueue.)
        ref (java.lang.ref.WeakReference. (Object.) rq)]
    (System/gc)
    (if-let [r (.remove rq 5000)]
      (assert (identical? r ref))
      (println "GC timeout"))))
  
(defn -main [ & args ]
  (set! *warn-on-reflection* true)
  (dotimes [_ 10] ;; run a few iterations to allow JIT
    ;;(println "===")
    ;;(print "Orig: ") ;; 2187 msecs
    ;; (time (dotimes [_ 100] (orig-parse-schema example-schema)))
    (gc)
    (print "Java: ") ;; ~3.5 msecs (~51 msecs before JIT)
    (time (dotimes [_ 100] (parse-schema example-schema)))))

