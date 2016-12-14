(ns graphql-clj.spec.type-system
  (:require [clojure.spec :as s]))

;; # Type system
(s/def :graphql-clj/type-system
  (s/keys :req [:graphql-clj/type-system-definitions]))

(s/def :graphql-clj/type-system-definitions
  (s/coll-of :graphql-clj/node))

;; (s/def :graphql-clj/node-type #{:schema-definition
;;                                 :type-definition
;;                                 :type-field
;;                                 :type-field-argument})

(s/def :graphql-clj/type-definition
  (s/keys :req [:graphql-clj/node-type]))

(s/def :graphql-clj/schema-definition
  (s/keys :req [:graphql-clj/node-type]))

(defmulti node-type :graphql-clj/node-type)

(defmethod node-type :graphql-clj/schema-definition [_]
  (s/keys :req [:graphql-clj/node-type]))

(defmethod node-type :graphql-clj/type-definition [_]
  (s/keys :req [:graphql-clj/node-type]))

(defmethod node-type :graphql-clj/tyep-field [_]
  (s/keys :req [:graphql-clj/node-type]))

(defmethod node-type :graphql-clj/type-field-argument [_]
  (s/keys :req [:graphql-clj/node-type]))

(s/def :graphql-clj/node (s/multi-spec node-type :graphql-clj/node-type))

