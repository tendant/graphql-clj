(ns graphql-clj.introspection-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [graphql-clj.executor :as executor]
            [graphql-clj.validator :as validator]
            [graphql-clj.introspection :as intro]))

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
  (-> type-spec parser/parse validator/validate-schema :state :schema)) ;; TODO don't unwrap

(deftest test-schema-introspection
  (let [schema (create-test-schema user-schema-str)
        resolver-fn nil
        context nil
        query "query { __schema { types {name kind} }}"
        result (executor/execute context schema resolver-fn query)]
    (is (= (-> result :data (update-in ["__schema" "types"] set))
           {"__schema" {"types" #{{"name" "QueryRoot" "kind" :OBJECT}
                                  {"name" "User" "kind" :OBJECT}
                                  {"name" "String" "kind" :SCALAR}
                                  {"name" "Int" "kind" :SCALAR}
                                  {"name" "Float" "kind" :SCALAR}
                                  {"name" "Boolean" "kind" :SCALAR}
                                  {"name" "ID" "kind" :SCALAR}
                                  {"name" "__Schema" "kind" :OBJECT}
                                  {"name" "__Type" "kind" :OBJECT}
                                  {"name" "__TypeKind" "kind" :ENUM}
                                  {"name" "__Field" "kind" :OBJECT}
                                  {"name" "__InputValue" "kind" :OBJECT}
                                  {"name" "__EnumValue" "kind" :OBJECT}
                                  {"name" "__Directive" "kind" :OBJECT}
                                  {"name" "__DirectiveLocation" "kind" :ENUM}}}}))))

(deftest test-schema-introspection-without-user-schema
  (let [schema validator/introspection-schema
        result (executor/execute nil schema nil intro/introspection-query)]
    (is (not (nil? (:data result))))))
