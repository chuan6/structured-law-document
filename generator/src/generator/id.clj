(ns generator.id
  (:require [clj-http.util :as http]
            [clojure.string :as str]
            [clojure.test :as t]
            [generator.test :as tt]
            [generator.tokenizer :as tk]))

(defn encode-id [s]
  (str/replace (http/url-encode s) #"%" "."))

(defn entry-id [context t]
  (letfn [(gen-str [context t]
            (let [r (str (name t) (t context))]
              (case t
                :则 r
                :章 r
                :节 (str (gen-str context :章) r)
                :条 r
                :款 (str (gen-str context :条) r)
                :项 (str (gen-str context :款) r)
                :目 (str (gen-str context :项) r))))]
    (encode-id (gen-str context t))))

(defn generate-id
  {:test
   #(let [f generate-id]
      (tt/comprehend-tests
       (t/is (= ""          (f {} [])))
       (t/is (= "条1"       (f {} [{:token :法 :nth :this}
                                   {:token :条 :nth 1}])))
       (t/is (= "条1"       (f {:条 1} [{:token :条 :nth :this}])))
       (t/is (= "条1款2项3" (f {} [{:token :法 :nth :this}
                                   {:token :条 :nth 1}
                                   {:token :款 :nth 2}
                                   {:token :项 :nth 3}])))
       (t/is (= "条1款1项5") (f {:条 1 :款 2} [{:token :款 :nth :prev}
                                               {:token :项 :nth 5}]))))}
  [context src]
  (str/join
   (loop [r () s src]
     (if (empty? s)
       r
       (let [c (peek s)]
         (if (= (count s) 1)
           (let [c-t (:token c)]
             (assert (t/is ((set (vals tk/adj->nth)) (:nth c))))
             (if (#{:法 :规定} c-t)
               r
               (let [r' (into r [(({:this identity
                                    :prev dec} (:nth c)) (context c-t)) (tk/item-type-str c)])]
                 (if (= c-t :款)
                   (recur r' [{:token :条 :nth :this}])
                   r'))))
           (recur (into r [(:nth c) (tk/item-type-str c)]) (pop s))))))))
