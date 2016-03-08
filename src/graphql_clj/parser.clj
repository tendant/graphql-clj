(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(comment
  ((insta/parser (io/resource "graphql.bnf") :auto-whitespace whitespace) "query {
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}
")
  )
