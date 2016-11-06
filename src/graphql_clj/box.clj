(ns graphql-clj.box)

(defn- val->box
  "If we already have a type that can preserve metadata, use it.
   Otherwise, wrap the value in a :v/boxed couple."
  [v m]
  (if (or (map? v) (vector? v))
    (with-meta v m)
    (with-meta [:v/boxed v] m)))

(defn kv->box
  "Given a single key value pair in a map, preserve metadata for the value."
  [value]
  (let [m (meta value)
        [[k v]] (vec value)]
    {k (val->box v m)}))

(defn box->val
  "Given a potentially boxed value, strip the box and return the raw value.
   Same as identity if value is already unboxed"
  [v]
  (if (and (vector? v) (= :v/boxed (first v)))
    (last v)
    v))
