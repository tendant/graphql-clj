(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as sv]
            [graphql-clj.query-validator :as qv]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn args [arguments vars]
  (printf "args: arguments: %s, vars: %s.%n" arguments vars)
  {"words" 1})

(defn- resolve-field-on-object
  [{:keys [resolver-fn name arguments] :as field-entry} {:keys [context resolver vars] :as state} parent-type-name parent-result]
  (printf "** resolve-field-on-object: field-entry: %s.%n" field-entry)
  (printf "** resolve-field-on-object: name: %s, parent-type-name: %s. parent-result: %s%n." name parent-type-name parent-result)
  (printf "** resolve-field-on-object: resolver-fn: %s, fn:%s.%n" resolver-fn (resolver parent-type-name name))
  (let [resolve (or resolver-fn (resolver (str parent-type-name) (str name)))
        ]
    (assert resolve (format "Resolver is nil: parent-type-name:%s, name:%s." parent-type-name name))
    (resolve context parent-result (args arguments vars))))

(declare execute-fields)

(defn- complete-value
  [{:keys [selection-set name type required] :as field-entry} state result]
  (printf "complete-value: field-entry: %s, type: %s, result: %s%n" field-entry type result)
  (when (and required (nil? result))
    (gerror/throw-error (format "NOT_NULL field \"%s\" assigned a null value." name)))
  (let [type-name (:name type)
        kind (:kind type)
        of-kind (:of-kind type)]
    (when result
      (cond
        (#{:SCALAR :ENUM} kind)             result
        (#{:OBJECT :INTERFACE :UNION} kind) (execute-fields selection-set state type-name result)
        (#{:LIST} kind)                     (map #(complete-value (merge field-entry of-kind) state %) result)
        :else (gerror/throw-error (format "Unknow field(%s) kind: %s%n" name kind))))))

(defn- get-field-entry [{:keys [name field-name] :as field-entry} state parent-type-name parent-result]
  (printf "get-field-entry: field-entry: %s, parent-result: %s%n" field-entry parent-result)
  [(str (or name field-name)) (->> (resolve-field-on-object field-entry state parent-type-name parent-result)
                             (complete-value field-entry state))])

(defn- get-field-type
  [schema parent-type-name field-name]
  (let [parent-type (get-in schema [:type-map parent-type-name])
        field-map (:field-map parent-type)
        field-type-name (get-in field-map [field-name :type :name])
        field-type (get-in schema [:type-map field-type-name])]
    (if field-type
      field-type
      (gerror/throw-error (format "get-field-type: unknow field-type for field(%s : %s)%n" parent-type-name field-name)))))

(defn- execute-field
  [selection field-map state parent-type-name parent-value]
  (let [selection-name (:name selection)
        field (get field-map selection-name)]
    (assert selection-name (format "No name for selection: %s." selection))
    (assert field (format "No field found for selection: %s." selection))
    (get-field-entry selection state parent-type-name parent-value)))

(defn- execute-fields
  [selection-set {:keys [schema] :as state} parent-type-name parent-value]
  (printf "execute-fields: selection-set:%s%n" selection-set)
  (let [parent-type (get-in schema [:type-map parent-type-name])
        field-map (:field-map parent-type)]
    (printf "execute-fields: parent-type: %s%n" parent-type)
    (->> selection-set
         (map (fn [field] (assoc field :type (get-field-type schema parent-type-name (:name field)))))
         (map #(execute-field % field-map state parent-type-name parent-value))
         (into {}))))

(defn- guard-missing-vars! [{:keys [variable-definitions]} vars]
  (let [required-vars (->> (remove :default-value variable-definitions) (map :variable-name) set)
        input-vars    (set (map name (keys vars)))
        missing-vars  (set/difference required-vars input-vars)]
    (when-not (empty? missing-vars)
      (gerror/throw-error (format "Missing input variables (%s)." (str/join "," missing-vars))))))

(defn- execute-statement [{:keys [selection-set] :as statement} {:keys [variables] :as state}]
  (guard-missing-vars! statement variables)
  (execute-fields selection-set state 'QueryRoot :root))

(defn- execute-document
  [document state]
  (printf "execute-document: document: %s%n" document)
  {:data (into {} (map #(execute-statement % state) document))})

;; Public API

(defn execute-validated-document
  ([context [schema-errors schema] resolver-fn [statement-errors document] variables]
   (if (seq schema-errors)
     (gerror/throw-error "Schema validation error" schema-errors))
   (if (seq statement-errors)
     {:errors statement-errors}
     (execute-document document {:variables variables
                                 :context context
                                 :schema schema
                                 :resolver (resolver/create-resolver-fn schema resolver-fn)})))
  ([context validated-schema resolver-fn validated-document]
   (execute-validated-document context validated-schema resolver-fn validated-document nil)))

(defn execute
  ([context string-or-validated-schema resolver-fn string-or-validated-document variables]
   (let [validated-schema (if (string? string-or-validated-schema)
                            (-> (parser/parse-schema string-or-validated-schema)
                                (sv/validate-schema))
                            string-or-validated-schema)
         valid-schema (second validated-schema)
         validated-document (if (string? string-or-validated-document)
                              (->> (parser/parse-query-document string-or-validated-document)
                                   (qv/validate-query valid-schema))
                              string-or-validated-document)]
     (execute-validated-document context validated-schema resolver-fn validated-document variables)))
  ([context validated-schema resolver-fn string-or-validated-document]
   (execute context validated-schema resolver-fn string-or-validated-document nil)))


;; testing data
(def test-schema
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

type QueryRoot {
  dog: Dog
}")

(def test-query-1
  "query getDogName {
  dog {
    name
  }
}")

(def test-validated-schema (-> test-schema
                               parser/parse-schema
                               sv/validate-schema))

(def test-validated-document (-> test-query-1
                                  parser/parse-query-document
                                  ((fn [s]
                                     [nil s]))))

(defn test-resolver-fn [type-name field-name]
  (printf "resolver-fn: type-name: %s, field-name: %s%n" type-name field-name)
  (let [f (get-in {"QueryRoot" {"dog" (fn [& args]
                                        {:name "Test Dog 1"})}}
                  [(name type-name) (name field-name)])]
    (printf "resolver fn: %s%n" f)
    (if f
      (println (f)))
    f))

;; (execute nil test-validated-schema test-resolver-fn test-validated-statement nil)
