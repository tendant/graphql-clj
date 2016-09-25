(ns graphql-clj.validator.rules.default-values-of-correct-type)

(defn default-for-required-arg-error [var-name type guess-type]
  (format "%s of type %s is required and will not use the default value. Perhaps you meant to use type %s."
          var-name
          type
          guess-type))

(defn default-for-required-field? [required? default-value]
  (and required? default-value))

(defn validate [schema ast]

  )
