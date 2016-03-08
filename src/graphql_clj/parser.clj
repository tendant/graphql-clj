(ns graphql-clj.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(comment
  ((insta/parser (io/resource "graphql.bnf")) "query {
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}
")
  
  "query {
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}"

  "{
  user(id: 4) {
    name
  }
}"

  "mutation {
  likeStory(storyID: 12345) {
    story {
      likeCount
    }
  }
}"

  "{
  me {
    id
    firstName
    lastName
    birthday {
      month
      day
    }
    friends {
      name
    }
  }
}"
  )
