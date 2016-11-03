(ns generator.punct
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]))

(def symbol->label {\: :colon
                    \： :colon
                    \; :semi-colon
                    \； :semi-colon
                    \, :comma
                    \， :comma
                    \、 :enum-comma
                    \. :period
                    \。 :period})

(def wave-dash "\u301c")

(def listing-seperators
  #{:comma :semi-colon :enum-comma})

(defn use-chinese
  {:test
   #(let [f use-chinese]
      (tt/comprehend-tests
       [(t/is (= "（）"         (f "()")))
        (t/is (= "（六）"       (f "(六)")))
        (t/is (= "六）"         (f "六)")))
        (t/is (= "六）（七"     (f "六)（七")))
        (t/is (= "组成："       (f "组成:")))
        (t/is (= "（的）组成：" (f "（的)组成:")))]))}
  [s]
  (let [f {":" "："
           ";" "；"
           "(" "（"
           ")" "）"
           "," "，"}]
    (str/replace s #"[\:\;\(\)\,]" #(f %1))))

(defn get-seq [s]
  (remove nil? (map symbol->label s)))
