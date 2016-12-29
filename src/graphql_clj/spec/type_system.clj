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
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/type-name
                :graphql-clj/type-fields
                :graphql-clj/kind]
          :opt [:graphql-clj/type-field-arguments]))

(s/def :graphql-clj/type-name string?)

(s/def :graphql-clj/type-fields
  (s/coll-of :graphql-clj/type-field))

(s/def :graphql-clj/type-field
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/field-name
                :graphql-clj/type-name]))

(s/def :graphql-clj/field-name string?)

(s/def :graphql-clj/type-field-arguments
  (s/coll-of :graphql-clj/type-field-argument))

(s/def :graphql-clj/type-field-argument
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/argument-name
                :graphql-clj/type-name]))

(s/def :graphql-clj/schema-definition
  (s/keys :req [:graphql-clj/node-type]))

(defmulti node-type :graphql-clj/node-type)

(defmethod node-type :graphql-clj/schema-definition [_]
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/query-type
                :graphql-clj/kind]))

(s/def :graphql-clj/kind #{:OBJECT
                           :INPUT_OBJECT
                           :UNION
                           :ENUM
                           :INTERFACE
                           :DIRECTIVE
                           :SCHEMA})

(s/def :graphql-clj/query-type
  (s/keys :req [:graphql-clj/name]))

(defmethod node-type :graphql-clj/type-definition [_]
  
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/type-name
                :graphql-clj/type-fields
                :graphql-clj/kind]
          :opt [:graphql-clj/type-field-arguments]))

(s/def :graphql-clj/type-fields
  (s/coll-of :graphql-clj/type-field))

(s/def :graphql-clj/type-field
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/field-name
                :graphql-clj/type-name]))

(s/def :graphql-clj/field-name string?)

(s/def :graphql-clj/type-field-arguments
  (s/coll-of :graphql-clj/type-field-argument))

(s/def :graphql-clj/type-field-argument
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/argument-name
                :graphql-clj/type-name]))

(s/def :graphql-clj/schema-definition
  (s/keys :req [:graphql-clj/node-type]))

(defmulti node-type :graphql-clj/node-type)

(defmethod node-type :graphql-clj/schema-definition [_]
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/query-type
                :graphql-clj/kind]))

(s/def :graphql-clj/kind #{:OBJECT
                           :INPUT_OBJECT
                           :UNION
                           :ENUM
                           :INTERFACE
                           :DIRECTIVE
                           :SCHEMA})

(s/def :graphql-clj/query-type
  (s/keys :req [:graphql-clj/name]))

(defmethod node-type :graphql-clj/type-definition [_]
  :graphql-clj/type-definition)

(defmethod node-type :graphql-clj/tyep-field [_]
  ;; (s/keys :req [:graphql-clj/node-type])
  :graphql-clj/type-field)

(defmethod node-type :graphql-clj/type-field-argument [_]
  :graphql-clj/type-field-argument)

(defmethod node-type :graphql-clj/enum-definition [_]
  :graphql-clj/enum-definition)

(s/def :graphql-clj/enum-definition
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/type-name
                :graphql-clj/enum-fields
                :graphql-clj/kind]))

;; TODO
;; (s/def :graphql-clj/enum-fields)

(defmethod node-type :graphql-clj/interface-definition [_]
  :graphql-clj/interface-definition)

(s/def :graphql-clj/interface-definition
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/type-name
                :graphql-clj/type-fields
                :graphql-clj/kind]))

(defmethod node-type :graphql-clj/union-definition [_]
  :graphql-clj/union-definition)

(s/def :graphql-clj/union-definition
  (s/keys :req [:graphql-clj/node-type
                :graphql-clj/type-name
                :graphql-clj/type-names]))

(s/def :graphql-clj/type-names
  (s/coll-of :graphql-clj/type-name))

(s/def :graphql-clj/node (s/multi-spec node-type :graphql-clj/node-type))

