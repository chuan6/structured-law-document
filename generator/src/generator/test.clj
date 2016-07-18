(ns generator.test)

(defmacro comprehend-tests
  ([] nil)
  ([tests & more-tests]
   `(when-not (empty? (concat (remove boolean (flatten ~tests))
                              (comprehend-tests ~@more-tests)))
      (throw (Exception.)))))
