(ns generator.test
  (:require [clojure.pprint :refer [pprint]]))

(defmacro comprehend-tests
  ([] nil)
  ([tests & more-tests]
   `(when-not (empty? (concat (remove boolean (flatten ~tests))
                              (comprehend-tests ~@more-tests)))
      (throw (Exception.)))))

(defn print-and-ret [x]
  (pprint x)
  x)
