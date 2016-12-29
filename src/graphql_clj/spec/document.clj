(ns graphql-clj.spec.document
  (:require [clojure.spec :as s]))

;; # query document
(s/def :graphql-clj/document
  (s/keys :req [:graphql-clj/operation-definitions]))

;; # operations
(s/def :graphql-clj/operation-definitions
  (s/coll-of :graphql-clj/operation-definition))

(s/def :graphql-clj/operation-definition
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/operation-type]))
