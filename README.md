# graphql-clj

A Clojure library designed to provide GraphQL implementation.

[![Build Status](https://travis-ci.org/tendant/graphql-clj.svg?branch=master)](https://travis-ci.org/tendant/graphql-clj)

## Demo

[Demo Project with GraphiQL](https://github.com/tendant/graphql-clj-starter)

## Project Status

This library is in the early stage. It is still under **active** development. All functions are subject to change. Please file an issue or submit a PR if you have suggestions!

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
        * [ ] List type variable
    * [ ] Union
    * [ ] Interface
    * [ ] Safe parallel execution
    * [ ] Coerce
- [x] Fragment execution
    * [ ] Fragment Type
- [x] Support Context
- [x] Type Introspect
    * [x] Type introspection schema (http://graphql.org/docs/introspection/)
    * [x] Type introspection query (http://facebook.github.io/graphql/#sec-Introspection)
    * [x] Arguments
    * [ ] Input Variables
    * [ ] Interfaces
- [ ] Directives
- [x] Testing
    * [ ] Use clojure.spec to generate test resolver automatically
- [X] Schema validation
- [X] Query validation
    * [ ] Overlapping fields can be merged (https://github.com/graphql/graphql-js/blob/master/src/validation/rules/OverlappingFieldsCanBeMerged.js)
- [X] Arguments validation
    * [ ] Argument Coerce
- [X] Variables validation
    * [ ] Variable Coerce
- [X] Parser error handling
- [ ] Execution error handling
- [ ] Batch data loading
- [ ] Comment as meta data
    * https://github.com/facebook/graphql/issues/200
    * https://github.com/graphql-java/graphql-java/issues/183
- [ ] Relay support
    * https://facebook.github.io/relay/docs/graphql-relay-specification.html
    * https://facebook.github.io/relay/graphql/connections.htm
    * https://facebook.github.io/relay/graphql/objectidentification.htm
    * https://facebook.github.io/relay/graphql/mutations.htm

## Installation

Add the following dependency to your project.clj file:

    [graphql-clj "0.1.18"]

This library uses clojure.spec for validation.  If you are not yet using clojure 1.9:

```
:dependencies [[org.clojure/clojure "1.8.0"]
               [graphql-clj "0.1.18" :exclusions [org.clojure/clojure]]
               [clojure-future-spec "1.9.0-alpha14"]]
```

## Usage

### Define schema

```clojure
(require '[graphql-clj.parser :as parser])
(require '[graphql-clj.type :as type])
(require '[graphql-clj.validator :as validator])

(def schema-str "type User {
    name: String
    age: Int
  }
  type QueryRoot {
    user: User
  }

  schema {
    query: QueryRoot
  }")

(def type-schema (-> schema-str parser/parse validator/validate-schema))
```

### Define resolver functions

```clojure
    (defn resolver-fn [type-name field-name]
      (cond
        (and (= "QueryRoot" type-name) (= "user" field-name)) (fn [context parent args]
                                                                {:name "test user name"
                                                                 :age 30})))
```
### Execute query
```clojure
    (require '[graphql-clj.executor :as executor])
    (def query-str "query {user {name age}}")
    (def context nil)
    (def prepared (executor/prepare type-schema resolver-fn query-str))
    
    (executor/execute context prepared)

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
