(ns graphql-clj.box)

(defn- val->box [v m]
  (with-meta [:v/boxed v] m))

(defn kv->box [value]
  (let [m (meta value)
        [[k v]] (vec value)]
    {k (val->box v m)}))

(defn box->val [v]
  (if (and (vector? v) (= :v/boxed (first v)))
    (last v)
    v))
