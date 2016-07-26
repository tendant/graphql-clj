(ns graphql-clj.introspection-test
  (:require [clojure.test :refer :all]
            [graphql-clj.type :as type]
            [graphql-clj.parser :as parser]
            [graphql-clj.resolver :as resolver]
            [graphql-clj.executor :as executor]))

(def simple-user-schema
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

(defn- create-test-schema
  [type-spec]
  (-> type-spec
      (parser/parse)
      (parser/transform)
      (type/create-schema)))


(deftest test-schema-introspection
  (let [schema (create-test-schema simple-user-schema)
        resolver-fn (resolver/create-resolver-fn schema nil)
        context nil
        document (parser/transform (parser/parse "query { __schema { types }}"))
        result (executor/execute context schema resolver-fn document)]
    (is (not (nil? (:data result))))))
