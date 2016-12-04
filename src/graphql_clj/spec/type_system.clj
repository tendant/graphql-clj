(ns graphql-clj.spec.type-system
  (:require [clojure.spec :as s]))

;; # Type system
(s/def :graphql-clj/type-system
  (s/keys :req [:graphql-clj/type-system-definitions]))
