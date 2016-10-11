### Hello World Example

```clojure
(require '[graphql-clj.parser :as parser])
(require '[graphql-clj.type :as type])

(def parsed-schema (parser/parse "type Query {
    hello: String
  }"))

(def type-schema (type/create-schema parsed-schema))

(defn resolver-fn [type-name field-name]
  (cond
    (and (= "Query" type-name) (= "hello" field-name)) (fn [context parent & rest]
                                                         "Hello world!")))

(require '[graphql-clj.executor :as executor])

(executor/execute nil type-schema resolver-fn "{ hello }")
```
