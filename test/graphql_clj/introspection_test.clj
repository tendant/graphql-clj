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
    (is (= {:data {"__type" {"name" "QueryRoot", "kind" :OBJECT, "description" "QueryRoot sample description"}}}
           result))))

(deftest introspection-query-type-fields
  (let [result (executor/execute nil " #QueryRoot sample description
type QueryRoot { name: String }" (constantly nil) "query {__type(name: \"QueryRoot\") {name kind description}}")]
    (is (not (:errors result)))
    (is (= {:data {"__type" {"name" "QueryRoot", "kind" :OBJECT, "description" "QueryRoot sample description"}}}
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

(deftest-valid-introspection query-type-fields-args-with-default-value
  "# QueryRoot sample description
type QueryRoot {
  # description for name field
  name(id: String): String
  fieldArgWithDefault(abc: String = \"Default abc\"): String
}"
  "query {__type(name: \"QueryRoot\") { name fields{name args{name defaultValue} type{name kind ofType{ name kind}}} }}"
  {:data
 {"__type"
  {"name" "QueryRoot",
   "fields"
   [{"name" '__typename,
     "args" [],
     "type"
     {"name" nil,
      "kind" :NON_NULL,
      "ofType" {"name" 'String, "kind" :SCALAR}}}
    {"name" 'name,
     "args" [{"name" 'id, "defaultValue" nil}],
     "type" {"name" 'String, "kind" :SCALAR, "ofType" nil}}
    {"name" 'fieldArgWithDefault,
     "args" [{"name" 'abc, "defaultValue" "Default abc"}],
     "type" {"name" 'String, "kind" :SCALAR, "ofType" nil}}
    {"name" '__schema,
     "args" [],
     "type"
     {"name" nil,
      "kind" :NON_NULL,
      "ofType" {"name" '__Schema, "kind" :OBJECT}}}
    {"name" '__type,
     "args" [{"name" 'name, "defaultValue" nil}],
     "type" {"name" '__Type, "kind" :OBJECT, "ofType" nil}}]}}})

(deftest-valid-introspection query-type-fields-args-list-type
  "# QueryRoot sample description
type QueryRoot {
  # description for name field
  name(id: String): String
  fieldArgWithDefault(abc: [String]): String
}"
  "query {__type(name: \"QueryRoot\") { name fields{name type{name kind ofType{name kind}} args {name type {name kind ofType{name kind} }}}  }}"
  {:data
   {"__type"
    {"name" "QueryRoot",
     "fields"
     [{"name" '__typename,
       "type"
       {"name" nil,
        "kind" :NON_NULL,
        "ofType" {"name" 'String, "kind" :SCALAR}},
       "args" []}
      {"name" 'name,
       "type" {"name" 'String, "kind" :SCALAR, "ofType" nil},
       "args"
       [{"name" 'id,
         "type" {"name" 'String, "kind" :SCALAR, "ofType" nil}}]}
      {"name" 'fieldArgWithDefault,
       "type" {"name" 'String, "kind" :SCALAR, "ofType" nil},
       "args"
       [{"name" 'abc,
         "type"
         {"name" nil,
          "kind" :LIST,
          "ofType" {"name" 'String, "kind" :SCALAR}}}]}
      {"name" '__schema,
       "type"
       {"name" nil,
        "kind" :NON_NULL,
        "ofType" {"name" '__Schema, "kind" :OBJECT}},
       "args" []}
      {"name" '__type,
       "type" {"name" '__Type, "kind" :OBJECT, "ofType" nil},
       "args"
       [{"name" 'name,
         "type"
         {"name" nil,
          "kind" :NON_NULL,
          "ofType" {"name" 'String, "kind" :SCALAR}}}]}]}}})

(deftest-valid-introspection query-type-enum
  "# Enum description
enum Episode {
  # newhope desc
  NEWHOPE,
  # empire desc
  EMPIRE, JEDI }

type QueryRoot {
}"
  "query {__type(name: \"Episode\") { name enumValues {name description} }}"
  {:data
   {"__type"
    {"name" "Episode",
     "enumValues"
     [{"name" 'NEWHOPE, "description" "newhope desc"}
      {"name" 'EMPIRE, "description" "empire desc"}
      {"name" 'JEDI, "description" nil}]}}})
