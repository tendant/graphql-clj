(ns graphql-clj.executor
  (:require [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as sv]
            [graphql-clj.query-validator :as qv]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.error :as gerror]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn arg-fn
  [default-args vars]
  (fn [argument]
    [(str (:name argument))
     (or (get-in argument [:value :value])
         (when (= :variable-reference (get-in argument [:value :tag]))
           (assert (get-in argument [:value :name]) "No name for variable reference!")
           (get vars (str (get-in argument [:value :name]))))
         (get default-args (str (:name argument))))]))

(defn args [arguments default-arguments vars]
  (let [default-args (->> default-arguments
                          (filter (fn [argument]
                                    (if (get-in argument [:default-value :value])
                                      true)))
                          (map (fn [argument]
                                 [(str (:name argument))
                                  (get-in argument [:default-value :value])]))
                          (into {}))
        args (->> arguments
                  (map (arg-fn default-args vars))
                  (into {}))]
    (merge default-args args)))

(defn- resolve-field-on-object
  [{:keys [resolver-fn name arguments] :as field-entry} field-def {:keys [context resolver variables] :as state} parent-type-name parent-result]
  (assert field-entry (format "field-entry is nil! parent-type-name: %s." parent-type-name))
  (assert field-def (format "field-def is nil!"))
  (let [resolve (or resolver-fn
                    (resolver (str parent-type-name) (str name)))
        default-arguments (:arguments field-def)]
    (assert resolve (format "Resolver is nil: parent-type-name:%s, name:%s." parent-type-name name))
    (resolve context parent-result (args arguments default-arguments variables))))

(declare execute-fields)

(defn- complete-value
  [{:keys [selection-set name type required] :as field-entry} {:keys [schema] :as state} result]
  (when (and required (nil? result))
    (gerror/throw-error (format "NOT_NULL field \"%s\" assigned a null value." name)))
  (assert type (format "type if nil for field(%s)." name))
  (let [type-name (:name type)
        tag (:tag type)
        inner-type (:inner-type type)]
    (when result
      (cond
        (#{:scalar-definition :union-definition :enum-definition} tag) result
        (#{:type-definition} tag) (execute-fields selection-set state type-name result)
        (#{:interface-definition} tag) (execute-fields selection-set state type-name result)
        (#{:basic-type} tag) (let [unwrapped-type (get-in schema [:type-map type-name])]
                               (complete-value (assoc field-entry :type unwrapped-type) state result))
        (#{:list-type} tag) (do
                              (let [list-result (map #(complete-value {:selection-set selection-set
                                                                       :type inner-type
                                                                       :require (:required type)} state %) result)]
                                list-result))
        :else (gerror/throw-error (format "Unhandled field(%s) type: %s%n field-entry:%s%n" name type field-entry))))))

(defn- get-field-entry [{:keys [alias name field-name] :as selection} field-def state parent-type-name parent-result]
  (assert selection "selection is nil!")
  (assert field-def (format "field-def is nil for selection: %s." selection))
  [(str (or alias name field-name)) (->> (resolve-field-on-object selection field-def state parent-type-name parent-result)
                             (complete-value selection state))])

(defn- get-field-type
  [schema parent-type-name field-name]
  (assert schema "Schema is nil!")
  (assert parent-type-name (format "Parente-type-name is nil for field: %s!" parent-type-name))
  (assert field-name "field-name is nil!")
  (let [parent-type (get-in schema [:type-map parent-type-name])
        field-map (:field-map parent-type)
        field (get field-map field-name)
        field-type-name (get-in field [:type :name])
        field-type (get-in schema [:type-map field-type-name])]
    (assert parent-type (format "Could not found parent-type for type name: %s." parent-type-name))
    (case (get-in field [:type :tag])
      :basic-type (get-in schema [:type-map field-type-name])
      :list-type (:type field)
      (gerror/throw-error (format "Unhandled field type: %s (name = %s)." field field-name)))))

(defn- execute-field
  [selection field-map state parent-type-name parent-value]
  (let [selection-name (:name selection)
        field-def (get field-map selection-name)]
    (assert selection-name (format "No name for selection: %s." selection))
    (assert field-def (format "No field found for selection: %s." selection))
    (get-field-entry selection field-def state parent-type-name parent-value)))

(defn- execute-fields
  [selection-set {:keys [schema] :as state} parent-type-name parent-value]
  (assert parent-type-name "parent type name is nil!")
  (let [parent-type (get-in schema [:type-map parent-type-name])
        field-map (:field-map parent-type)]
    (assert parent-type (format "parent type is nil for parent type: %s." parent-type-name))
    (->> selection-set
         (map (fn [field] (assoc field :type (get-field-type schema parent-type-name (:name field)))))
         (map #(execute-field % field-map state parent-type-name parent-value))
         (into {}))))

(defn- guard-missing-vars [variable-definitions vars]
  (let [required-var-names (->> (remove :default-value variable-definitions) (map :name) (map str) set)
        default-vars (->> (filter :default-value variable-definitions)
                          (map (fn [var-def]
                                 [(str (:name var-def))
                                  (get-in var-def [:default-value :value])]))
                          (into {}))
        input-var-names    (set (map key vars))
        missing-var-names  (set/difference required-var-names input-var-names)
        variables (merge default-vars vars)]
    [(map (fn erorr-msg [name] {:message (format "Missing input variables (%s)." name)}) missing-var-names)
     variables]))

(defn- execute-statement [{:keys [tag selection-set variable-definitions] :as statement} {:keys [variables schema] :as state}]
  (assert schema "Schema is nil in state!")
  (let [[errors updated-variables] (guard-missing-vars variable-definitions variables)
        root-type (case tag
                    :query-definition (get-in schema [:roots :query])
                    :selection-set (get-in schema [:roots :query]) ; anonymous default query
                    :mutation (get-in schema [:roots :mutation])
                    (gerror/throw-error (format "Unhandled statement type: %s" statement)))]
    (assert root-type "No root type found in schema.")
    (if (seq errors)
      [errors nil]
      [nil (execute-fields selection-set (assoc state :variables updated-variables) root-type :root)])))

(defn- execute-document
  [document state]
  (let [results (map #(execute-statement % state) document)
        errors (->> results
                    (filter first)
                    (map first)
                    flatten
                    vec)
        fields (->> results
                    (filter second)
                    (map second)
                    flatten
                    vec)]
    (if (seq errors)
      {:errors errors}
      {:data (into {} fields)})))

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

(defn validate-document [query-str]
  (->> query-str
       parser/parse-query-document
       (qv/validate-query (second test-validated-schema))))

(defn test-resolver-fn [type-name field-name]
  (let [f (get-in {"QueryRoot" {"dog" (fn [& args]
                                        {:name "Test Dog 1"})}}
                  [(name type-name) (name field-name)])]
    (if f
      (println (f)))
    f))

;; (execute nil test-validated-schema test-resolver-fn (validate-document test-query-1) nil)

(def test-intro-str
  "
  query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  }")
