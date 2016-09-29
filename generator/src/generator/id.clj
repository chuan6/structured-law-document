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

(def templates
  {:目 [:条 :款 :项]
   :项 [:条 :款]
   :款 [:条]
   :条 []
   :节 [:章]
   :章 []})

(defn- interpret-nth-with [context {ty :token nx :nth}]
  (if (number? nx)
    nx
    (do (assert (#{:this :prev} nx))
        (({:this identity
           :prev dec
           } nx)
         (ty context)))))

(defn generate
  {:test
   #(let [f generate]
      (tt/comprehend-tests
       (t/is (nil?    (f {} [])))
       (t/is (= "条1" (f {} [{:token :法 :nth :this}
                             {:token :条 :nth 1}])))
       (t/is (= "条1" (f {:条 1} [{:token :条 :nth :this}])))
       (t/is (= "条1款2项3"
                (f {} [{:token :法 :nth :this}
                       {:token :条 :nth 1}
                       {:token :款 :nth 2}
                       {:token :项 :nth 3}])))
       (t/is (= "条1款1项5"
                (f {:条 1 :款 2} [{:token :款 :nth :prev}
                                  {:token :项 :nth 5}])))))}
  ([context tv]
   (let [t (:token (peek tv))]
     (when (contains? templates t)
       (generate context tv t))))

  ([context tv expect]
   (let [interpret-nth (partial interpret-nth-with context)]
    (if (nil? expect)
      ""
      (let [{ty :token :as t} (peek tv)
            next-expect (peek (expect templates))]
        (cond (nil? t)                ; extend
              (str (generate context [] next-expect)
                   (name expect) (expect context))

              (= ty expect)
              (str (generate context (pop tv) next-expect)
                   (tk/item-type-str ty) (interpret-nth t))

              :mismatch
              (throw (Exception. (str "Expect " expect ", instead "
                                      "get " ty)))))))))
