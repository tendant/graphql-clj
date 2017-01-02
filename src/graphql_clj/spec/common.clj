(ns graphql-clj.spec.common
  (:require [clojure.spec :as s]))

(s/def :graphql-clj/name (s/and string? #(re-matches #"[_a-zA-Z][_0-9a-zA-z]*" %)))
