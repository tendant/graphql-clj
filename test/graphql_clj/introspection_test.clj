(ns graphql-clj.introspection-test
  (:require [clojure.test :refer :all]
            [graphql-clj.executor :as executor]
            [graphql-clj.schema-validator :as sv]
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

(def schema (sv/validate-schema schema-str))

(deftest schema-introspection
  (let [resolver-fn nil
        context nil
        query "query { __schema { types {name kind} }}"
        result (executor/execute context schema resolver-fn query)]
    (is (not (:errors result)))
    (is (= result
           {:data
            {"__schema"
             {"types"
              (list {"name" 'QueryRoot, "kind" :OBJECT}
                    {"name" '__EnumValue, "kind" :OBJECT}
                    {"name" 'String, "kind" :SCALAR}
                    {"name" 'Boolean, "kind" :SCALAR}
                    {"name" '__Field, "kind" :OBJECT}
                    {"name" '__Directive, "kind" :OBJECT}
                    {"name" '__Type, "kind" :OBJECT}
                    {"name" '__TypeKind, "kind" :ENUM}
                    {"name" 'User, "kind" :OBJECT}
                    {"name" 'Float, "kind" :SCALAR}
                    {"name" '__DirectiveLocation, "kind" :ENUM}
                    {"name" 'Int, "kind" :SCALAR}
                    {"name" '__InputValue, "kind" :OBJECT}
                    {"name" '__Schema, "kind" :OBJECT})}}}))))

;; (deftest schema-introspection-without-user-schema
;;   (let [intro-schema (-> intro/introspection-schema sv/validate-schema)
;;         result       (executor/execute nil intro-schema (constantly nil) intro/introspection-query)]
;;     (is (not (:errors result)))
;;     (is (= {"name" "Query"} (get-in result [:data "__schema" "queryType"])))))

(deftest schema-introspection-with-argument
  (let [query-str "query{ __type(name: \"User\") { name kind }}"
        result (executor/execute nil schema (constantly nil) query-str)]
    (is (not (:errors result)))))

(deftest introspection-query-type
  (let [result (executor/execute nil "# QueryRoot sample description
type QueryRoot { name: String }" (constantly nil) "query {__type(name: \"QueryRoot\") {name kind description}}")]
    (is (not (:errors result)))
    (is (= {:data {"__type" {"name" "QueryRoot", "kind" :OBJECT, "description" "# QueryRoot sample description"}}}
           result))))

(deftest introspection-query-type-fields
  (let [result (executor/execute nil "# QueryRoot sample description
type QueryRoot { name: String }" (constantly nil) "query {__type(name: \"QueryRoot\") {name kind description}}")]
    (prn result)
    (is (not (:errors result)))
    (is (= {:data {"__type" {"name" "QueryRoot", "kind" :OBJECT, "description" "# QueryRoot sample description"}}}
           result))))

(defmacro deftest-valid-introspection [name schema query expected]
  `(deftest ~name
     (let [expect# ~expected
           {:keys [data# errors#] :as actual#} (executor/execute nil ~schema (constantly nil) ~query)]
       (if (empty? errors#)
         (report {:type :pass})
         (report {:type :fail :expected [] :actual errors#}))
       (if (or (nil? expect#) (= expect# actual#))
         (report {:type :pass})
         (report {:type :fail :expected expect# :actual actual#})))))

(deftest-valid-introspection query-type-fields
  "# QueryRoot sample description
type QueryRoot { name: String }"
  "query {__type(name: \"QueryRoot\") { name fields{name} }}"
  {:data
   {"__type"
    {"name" "QueryRoot",
     "fields"
     [{"name" '__typename}
      {"name" 'name}
      {"name" '__schema}
      {"name" '__type}]}}})

;; ;; FIXME: name has no field of args, should return error
;; (deftest-valid-introspection query-type-fields-args-invalid
;;   "# QueryRoot sample description
;; type QueryRoot { name(id: String): String }"
;;   "query {__type(name: \"QueryRoot\") { name fields{name {args} } }}"
;;   {:data
;;    {"__type"
;;     {"name" "QueryRoot",
;;      "fields"
;;      [{"name" '__typename}
;;       {"name" 'name}
;;       {"name" '__schema}
;;       {"name" '__type}]}}})

(deftest-valid-introspection query-type-fields-args
  "# QueryRoot sample description
type QueryRoot {
  # description for name field
  name(id: String): String
}"
  "query {__type(name: \"QueryRoot\") { name fields{name args{name} type{name}} }}"
  {:data
   {"__type"
    {"name" "QueryRoot",
     "fields"
     [{"name" '__typename, "args" nil, "type" {"name" 'String}}
      {"name" 'name, "args" [{"name" 'id}], "type" {"name" 'String}}
      {"name" '__schema, "args" nil, "type" {"name" '__Schema}}
      {"name" '__type, "args" [{"name" 'name}], "type" {"name" '__Type}}]}}})
