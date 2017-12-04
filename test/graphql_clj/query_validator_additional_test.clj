(ns graphql-clj.query-validator-additional-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [graphql-clj.schema-validator :as schema-validator]
            [graphql-clj.query-validator :as query-validator]
            [graphql-clj.query-validator-util :refer [deftest-valid deftest-invalid]]))

(defn- err [msg sl sc si el ec ei & trace]
  (let [e {:message msg :start {:line sl :column sc :index si} :end {:line el :column ec :index ei}}]
    (if (empty? trace) e (assoc e :trace (into [] trace)))))

(def schema-issue-50-str "schema {
      query: Query
    }

    type Query {
      a: A
      x: X
    }

    type A {
      b: B
    }

    type B {
      c(foo: Int): String
    }

    type X {
      y(foo: Int): String
    }
    ")

(defn create-schema [schema-str]
  (schema-validator/validate-schema schema-str))

(def schema-issue-50 (create-schema schema-issue-50-str))

(deftest-valid schema-issue-50-valid schema-issue-50
  "{ a { b { c(foo: 1) } } }"
  [{:selection-set
    [{:tag :selection-field,
      :name 'a
      :selection-set
      [{:tag :selection-field
        :name 'b
        :selection-set
        [{:tag :selection-field
          :name 'c
          :arguments
          [{:tag :argument
            :name 'foo
            :value {:tag :int-value, :image "1", :value 1}}]
          :resolved-type {:tag :basic-type, :name 'String}}]
        :resolved-type {:tag :basic-type, :name 'B}}]
      :resolved-type {:tag :basic-type, :name 'A}}]
    :tag :selection-set}])


(def schema-issue-48-str "schema {
  query: Query
}

input TextInput {
  value: String
}

input WorldInput {
  text: TextInput
}

type Query {
  hello(world: WorldInput): String
}")

(def schema-issue-48 (create-schema schema-issue-48-str))

;; FIXME: unsupported by query parser
(deftest-valid schema-issue-48-valid schema-issue-48
  "{
  hello(world: {text: {value: \"World\"}})
}"
  [{:tag :selection-set, :selection-set
    [{:tag :selection-field, :name 'hello
      :arguments [{:tag :argument, :name 'world :value {:tag :object-value,
                                                        :fields [{:tag :object-field, :name 'text
                                                                  :value {:tag :object-value,
                                                                          :fields [{:tag :object-field, :name 'value :value {:tag :string-value, :image "\"World\"", :value "World"}}]}}]}}]
      :resolved-type {:tag :basic-type, :name 'String}}]}])

(def schema-no-operation-provided-in-document-with-enum "schema {
  mutation: Mutation
}

  type Mutation {
  testMethod(action: String, id: String!, pid: String): Status
}

  enum Status { DELETED, CONFIRMED }")

(deftest-invalid no-operation-provided-in-document-with-enum-invalid schema-no-operation-provided-in-document-with-enum
  "mutation {  participateActivity( action:\"JOIN\", activityId: \"fd2027fc-0888-416c-b422-7b3ee7671b7d\") } { id }"
  (err "Field selections on scalars are never allowed!" 1 103 102 1 109 108))
