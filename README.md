# graphql-clj

A Clojure library designed to provide GraphQL implementation.

## Project Status

This library is an **alpha** version. It is still under development. All functions are subject to change.

The implementation of the library follow closely to the GraphQL Draft RFC Specification (https://facebook.github.io/graphql/).

- [x] GraphQL parser
    * [x] Query
    * [x] Mutation
    * [x] Type System
    * [x] Variables
- [x] Transformation
- [x] Execution
    * [x] Query
    * [x] Mutation
    * [x] Support List Type
    * [x] Support Non-Null Type
    * [x] Arguments
    * [x] Variables
    * [ ] Union
    * [ ] Interface
    * [ ] Safe parallel execution
- [x] Fragment execution
    * [ ] Fragment Type
- [x] Support Context
- [ ] Type Introspect
    * [x] Type introspection schema (http://graphql.org/docs/introspection/)
    * [ ] Type introspection query (http://facebook.github.io/graphql/#sec-Introspection)
- [ ] Directives
- [x] Testing
- [ ] Schema validation
- [ ] Query validation
- [X] Arguments validation
- [ ] Variables validation
- [ ] Parser error handling
- [ ] Execution error handling
- [ ] Batch data loading
- [ ] Comment as meta data
    * https://github.com/facebook/graphql/issues/200
    * https://github.com/graphql-java/graphql-java/issues/183

## Installation

Add the following dependency to your project.clj file:

    [graphql-clj "0.1.7"]

## Usage

### Define schema

```clojure
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

(def type-schema (type/create-schema parsed-schema))
```

### Define resolver functions

```clojure
    (defn resolver-fn [type-name field-name]
      (cond
        (and (= "QueryRoot" type-name) (= "user" field-name)) (fn [context parent & args]
                                                                {:name "test user name"
                                                                 :age 30})))
```
### Execute query
```clojure
    (require '[graphql-clj.executor :as executor])
    (def query "query {user {name age}}")
    (def context nil)
    
    (executor/execute context type-schema resolver-fn query)

    ;; {:data {"user" {"name" "test user name", "age" 30}}}
```
## Deploy to local for development

    $ lein install

## Release to Clojars

    $ lein deploy clojars

## Test

    $ lein test

## License

Copyright © 2016 Lei Wang

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
