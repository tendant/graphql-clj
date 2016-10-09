(ns graphql-clj.validator.errors)

(defn- conj-error [error errors]
  (conj (or errors []) {:error error}))

(defn update-errors [ast error]
  (update ast :errors (partial conj-error error))) ;; TODO add :loc once parse spans are preserved
