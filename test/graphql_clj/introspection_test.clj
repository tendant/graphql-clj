(ns graphql-clj.introspection-test
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [graphql-clj.executor :as executor]
            [graphql-clj.introspection :as intro]
            [graphql-clj.box :as box]))

(def user-schema-str
  "type User {
  name: String
  nickname: String
  son: User
  friends: [User]
}

type QueryRoot {
  user: User
}

schema {
  query: QueryRoot
}")

(defn- create-test-schema [type-spec]
  (type/create-schema (parser/parse type-spec) intro/introspection-schema))

(deftest test-schema-introspection
  (let [schema (create-test-schema user-schema-str)
        resolver-fn nil
        context nil
        query "query { __schema { types {name kind} }}"
        result (executor/execute context schema resolver-fn query)]
    (is (= (-> result :data (update-in ["__schema" "types"] set))
           {"__schema" {"types" #{{"name" (box/->Box "QueryRoot" {}) "kind" :OBJECT} ;; TODO strip box after validate / before execute if needed
                                  {"name" (box/->Box "User" {}) "kind" :OBJECT}
                                  {"name" "String" "kind" :SCALAR}
                                  {"name" "Int" "kind" :SCALAR}
                                  {"name" "Float" "kind" :SCALAR}
                                  {"name" "Boolean" "kind" :SCALAR}
                                  {"name" "ID" "kind" :SCALAR}
                                  {"name" (box/->Box "__Schema" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__Type" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__TypeKind" {}) "kind" :ENUM}
                                  {"name" (box/->Box "__Field" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__InputValue" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__EnumValue" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__Directive" {}) "kind" :OBJECT}
                                  {"name" (box/->Box "__DirectiveLocation" {}) "kind" :ENUM}}}}))))

(deftest test-schema-introspection-without-user-schema
  (let [schema (type/create-schema intro/introspection-schema)
        result (executor/execute nil schema nil intro/introspection-query)]
    (is (not (nil? (:data result))))))
