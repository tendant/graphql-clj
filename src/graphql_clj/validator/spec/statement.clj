(ns graphql-clj.validator.spec.statement
  (:require [clojure.spec :as s]
            [graphql-clj.spec :as spec]
            [graphql-clj.validator :as validator]))

(s/def ::node-type #{:operation-definition :field :argument :variable-definition})

(s/def ::type #{"query" "mutation"})

(s/def ::operation-type (s/keys :req-un [::type]))

(s/def ::operation-definition (s/keys :req-un [::node-type ::operation-type ::selection-set]))

(s/def ::selection-set (s/coll-of ::field))

(s/def ::name string?)

(s/def ::field-name string?)

(s/def ::args-fn fn?)

(s/def ::resolver-fn fn?)

(s/def ::kind #{:SCALAR :OBJECT :LIST :UNION :INTERFACE :ENUM})

(s/def ::of-kind (s/keys :req-un [::kind] :opt-un [::of-kind ::required]))

(s/def ::required spec/boolean?*)

(s/def ::parent-type-name string?)

(s/def ::field
  (s/keys :req-un [::field-name ::node-type ::parent-type-name]
          :opt-un [::name ::args-fn ::resolver-fn ::required ::selection-set]))

(s/def ::operation-definitions (s/coll-of ::operation-definition))

(s/def ::document (s/keys :req-un [::operation-definitions]))

(s/def ::valid-statement (s/keys :req-un [::document]))

(s/def ::error string?)

(s/def ::line spec/int?*)

(s/def ::column spec/int?*)

(s/def ::loc  (s/keys :req-un [::line ::column]))

(s/def ::errors (s/coll-of (s/keys :req-un [::error] :opt-un [::loc])))

(s/def ::validation-output (s/or :valid   (s/keys :req-un [::document])
                                 :invalid (s/keys :req-un [::errors])))

(s/fdef validator/validate-statement :ret ::operation-definitions)
