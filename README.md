# graphql-clj

A Clojure library designed to provide GraphQL implementation.

## Project Status

    This library is an alpha version. It is still under development. All functions are subject to change.

    The implementation of the library follow closely to the GraphQL Draft RFC Specification (https://facebook.github.io/graphql/).

    [x] means complete implementation
    [-] means partial implementation

- [-] GraphQL parser
    1. [x] Query
    2. [ ] Mutation
    3. [x] Type System
    4. [ ] Variables
- [x] Transformation
- [-] Execution
    1. [-] Query
    2. [ ] Mutation
    3. [x] Support List Type
    4. [ ] Support Non-Null Type
    5. [ ] Arguments
- [-] Fragment execution
- [-] Support Context
- [-] Type Introspect
    http://graphql.org/docs/introspection/
    http://facebook.github.io/graphql/#sec-Introspection
- [ ] Directives
- [-] Testing
- [ ] Schema validation
- [ ] Query validation
- [ ] Parser error handling
- [ ] Batch data loading

## Installation

Add the following dependency to your project.clj file:

    [graphql-clj "0.1.5.-SNAPSHOT"]

## Usage

### Define schema

    (require '[graphql-clj.parser :as parser])
    (require '[graphql-clj.type :as type])

    (def parsed-schema (parser/parse "type User {
        name: String
        age: Int
      }
      type QueryRoot {
        user: User
      }

      schema {
        query: QueryRoot
      }"))

    (def transformed-schema (parser/transform parsed-schema))

    (def type-schema (type/create-schema transformed-schema))

### Define resolver functions

    (require '[graphql-clj.resolver :as resolver])

    (defn customized-resolver-fn [type-name field-name]
      (cond
        (and (= "QueryRoot" type-name) (= "user" field-name)) (fn [context parent & args]
                                                                {:name "test user name"
                                                                 :age 30})))

    (def resolver-fn (resolver/create-resolver-fn type-schema customized-resolver-fn))

### Execute query

    (require '[graphql-clj.executor :as executor])
    (def query-document (parser/transform (parser/parse "query {user {name age}}")))
    (def context nil)
    
    (executor/execute context type-schema resolver-fn query-document)

    ;; {:data {"user" {"name" "test user name", "age" 30}}}

## Test

    $ lein test

## License

Copyright © 2016 Lei Wang

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
