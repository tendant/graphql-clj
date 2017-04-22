(ns graphql-clj.query-validator-additional-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [graphql-clj.parser :as parser]
            [graphql-clj.schema-validator :as schema-validator]
            [graphql-clj.query-validator :as query-validator]
            [graphql-clj.query-validator-util :refer [deftest-valid]]))

(def schema-1-str "schema {
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

(def schema-1 (->> schema-1-str
                   parser/parse-schema
                   schema-validator/validate-schema
                   peek))

(deftest-valid schema-str-1-valid schema-1
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
