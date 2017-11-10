# graphql-clj

A Clojure library designed to provide GraphQL implementation.

[![Build Status](https://travis-ci.org/tendant/graphql-clj.svg?branch=master)](https://travis-ci.org/tendant/graphql-clj)

## Demo

[Demo Project with GraphiQL](https://github.com/tendant/graphql-clj-starter)

## What's new in version 0.2

1. Simplified APIs
2. Rewrite schema and query validator for simplicity and robostness.
3. Separate parser and validator for schema and query.
4. High performance java parser

## Installation

Add the following dependency to your project.clj file:

    [graphql-clj "0.2.6"]

## Usage

### Define schema

```clojure

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

```

### Define resolver functions

```clojure
(defn resolver-fn [type-name field-name]
  (get-in {"QueryRoot" {"user" (fn [context parent args]
                                 {:name "test user name"
                                  :age 30})}}
          [type-name field-name]))
```
### Execute query
```clojure
    (require '[graphql-clj.executor :as executor])
    (def query-str "query {user {name age}}")

    (executor/execute nil schema-str resolver-fn query-str)
    ;; => {:data {"user" {"name" "test user name", "age" 30}}}

```

### Caching validated schema and query for performance
```clojure
    (require '[graphql-clj.schema-validator :as schema-validator])
    (require '[graphql-clj.query-validator :as query-validator])
    
    ;; Consider memoizing the result of parsing and validating the query before execution
    (def validated-schema (schema-validator/validate-schema schema-str)) ; throw ex-info with ex-data {:errors errors}
    (def validated-query (query-validator/validate-query validated-schema query-str)) ; return [errors validated-ast]

    (executor/execute nil validated-schema resolver-fn validated-query)
    ;; => {:data {"user" {"name" "test user name", "age" 30}}}
```

### Migrating from 0.1.x to 0.2 version

1. Separated parser api for schema and query
```
   parser/parse-schema for schema parsing
   parser/parse-query-document for query parsing
```
2. Simplified validator api, it can take query string and schema string now.
```
   graphql-clj.schema-validator/validate-schema replaces validator/validate-schema
   graphql-clj.query-validator/validate-query replaces validator/validate-statement
```
3. executor/execute function can take string and validated result for both schema and query string.

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
