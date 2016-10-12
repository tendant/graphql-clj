(ns graphql-clj.validator.errors
  (:require [graphql-clj.spec :as spec]
            [clojure.spec :as s]
            [clojure.string :as str]
            [graphql-clj.error :as ge]))

(defn- conj-error [error errors]
  (conj (or errors []) {:error error}))

(defn update-errors [ast error]
  (update ast :errors (partial conj-error error))) ;; TODO add :loc once parse spans are preserved

(defn render-naked-object [v] ;; TODO render JSON and strip quotes - complex for the upside of matching CAT?
  (str/replace (pr-str (into {} (map (fn [[k v]] [(str (name k) ":") v]) v))) #"\"" ""))

(defn render [v] ;; TODO inconsistent and complex for the upside of matching CAT?
  (cond (string? v) (str "\"" v "\"")
        (map? v)    (render-naked-object v)
        :else       v))

(defn- missing-contains [spec containing-spec]              ;; TODO this is really complex
  (let [base-spec (s/get-spec (keyword (str (namespace containing-spec) "." (name containing-spec)) (name spec)))]
    (format "The NotNull field '%s' of type '%s' is missing" (name spec) (name base-spec))))

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
  (spec/validate-referenced-spec path spec)
  (s/valid? spec value))
