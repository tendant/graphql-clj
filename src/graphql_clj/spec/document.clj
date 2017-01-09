(ns graphql-clj.spec.document
  (:require [clojure.spec :as s]))

;; # query document
(s/def :graphql-clj/document
  (s/keys :req [:graphql-clj/operation-definitions]))

;; # operations
(s/def :graphql-clj/operation-definitions
  (s/coll-of :graphql-clj/operation-definition))

(s/def :graphql-clj/operation-type
  #{"mutation" "query" "subscription"})

(s/def :graphql-clj/operation-definition
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/operation-type]
          :opt [:graphql-clj/name
                :graphql-clj/selection-set]))

(s/def :graphql-clj/selection-set
  (s/coll-of :graphql-clj/selection))

(s/def :graphql-clj/selection
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/field-name]
          :opt [:graphql-clj/selection-set]))
