(ns graphql-clj.validator.errors
  (:require [graphql-clj.spec :as spec]
            [clojure.spec :as s]
            [clojure.string :as str]
            [graphql-clj.error :as ge]
            [graphql-clj.box :as box]))

(defn- conj-error [new-errors existing-errors]
  (into (or existing-errors []) (map #(if (map? %) % (hash-map :error %)) new-errors)))

(defn extract-loc
  [{:keys [instaparse.gll/start-line instaparse.gll/start-column]}]
  {:line start-line :column start-column})

(defn update-errors [ast & errors]
  (update ast :errors (partial conj-error errors)))

(defn render-naked-object [v] ;; TODO render JSON and strip quotes - complex for the upside of matching CAT?
  (str/replace (pr-str (into {} (map (fn [[k v]] [(str (name k) ":") v]) v))) #"\"" ""))

(defn render [v] ;; TODO inconsistent and complex for the upside of matching CAT?
  (cond (string? v) (str "\"" v "\"")
        (map? v)    (render-naked-object v)
        :else       v))

(defn unboxed-render [v] (-> v box/box->val render))

(defn- missing-contains [spec containing-spec]              ;; TODO this is really complex
  (let [base-spec (s/get-spec (keyword (str (namespace containing-spec) "." (name containing-spec)) (name spec)))]
    (format "The NotNull field '%s' of type '%s' is missing" (name spec) (spec/remove-required (name base-spec)))))

(def default-type-preds (set (vals spec/default-specs)))

(defn- explain-problem [spec {:keys [pred via] :as problem}] ;; TODO enum etc?
  (cond
    (= 'map? pred)
    (format "Expected '%s', found not an object" (name (s/get-spec spec)))

    (and (seq? pred) (= 'contains? (first pred)))
    (missing-contains (last pred) (s/get-spec (first via)))

    (default-type-preds pred)
    (format "%s value expected" (name (s/get-spec spec)))

    :else (ge/throw-error "Unhandled spec problem" {:spec spec :problem problem :base-spec (s/get-spec spec)})))

(defn explain-invalid [spec value] ;; TODO more complex than plumatic schema?
  (->> (s/explain-data spec value)
       :clojure.spec/problems
       (map (partial explain-problem spec))
       (str/join ",")))

(defn valid? [spec value path]
  (s/valid? spec value))

(defn duplicates
  "Return the duplicate values from a sequence"
  [s]
  (->> (frequencies s) (filter (fn [[_ v]] (> v 1))) keys))

(defn- duplicate-name-error [label duplicate-name]
  (format "There can be only one %s named '%s'."
          label
          (if (= "variable" label) (str "$" duplicate-name) duplicate-name)))

(defn duplicate-name-errors [label map-fn vals]
  (->> (map map-fn vals)
       duplicates
       (map (partial duplicate-name-error label))))

(defn guard-duplicate-names [label map-fn vals s]
  (let [errors (duplicate-name-errors label map-fn vals)]
    (when-not (empty? errors)
      {:state (apply update-errors s errors)
       :break true})))
