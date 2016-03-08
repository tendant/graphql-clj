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

  "# `me` could represent the currently logged in viewer.
{
  me {
    name
  }
}
"

  "# `me` could represent the currently logged in viewer.
{
  me {
    name
  }
}

# `user` represents one of many users in a graph of data, referred to by a
# unique identifier.
{
  user(id: 4) {
    name
  }
}"

  "{
  user(id: 4) {
    id
    name
    profilePic(size: 100)
  }
}
"
  "{
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}
"

  ;; In this example, we can fetch two profile pictures of different sizes and ensure the resulting object will not have duplicate keys:
  "{
  user(id: 4) {
    id
    name
    smallPic: profilePic(size: 64)
    bigPic: profilePic(size: 1024)
  }
}
"
  )
