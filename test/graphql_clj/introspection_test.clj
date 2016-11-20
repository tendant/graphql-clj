(ns graphql-clj.introspection-test
  (:require [clojure.test :refer :all]
            [graphql-clj.parser :as parser]
            [graphql-clj.executor :as executor]
            [graphql-clj.validator :as validator]
            [graphql-clj.introspection :as intro]))

(def schema-str
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

(def schema (-> schema-str parser/parse validator/validate-schema))

(deftest schema-introspection
  (let [resolver-fn nil
        context nil
        query "query { __schema { types {name kind} }}"
        result (executor/execute context schema resolver-fn query)]
    (is (not (:errors result)))
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

(deftest schema-introspection-without-user-schema
  (let [intro-schema (-> intro/introspection-schema validator/validate-schema)
        result       (executor/execute nil intro-schema (constantly nil) intro/introspection-query)]
    (is (not (:errors result)))
    (is (= {"name" "Query"} (get-in result [:data "__schema" "queryType"])))))

(deftest schema-introspection-with-argument
  (let [query-str "{ __type(name: \"User\") { name kind }}"
        result (executor/execute nil schema (constantly nil) query-str)]
    (is (not (:errors result)))))
