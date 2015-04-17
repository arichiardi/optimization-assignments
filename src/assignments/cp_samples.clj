(ns ^{:author "Andrea Richiardi"
      :doc "Some fun with Constraint Programming." }
  assignments.cp-samples
  (:refer-clojure :exclude [send])
  (:require [clojure.pprint :as pp :refer [pprint]]
            [loco.core :refer :all]
            [loco.constraints :refer :all]))

(set! *warn-on-reflection* true)

;; Solution to the SEND-MORE-MONEY problem
;; (def letters
;;   (for [letter "sendmory"]
;;     ($in [:l letter] 0 9)))

(defn costraint-0-9 [vars]
  (for [v vars]
    ($in v 0 9)))

(def send
  (for [l "send"]
    [:send l]))

(def more
  (for [l "more"]
    [:more l]))

(def money
  (for [l "money"]
    [:money l]))

(def letter-ties
  [;; letter e
   ($= [:send \e] [:more \e]) 
   ($= [:send \e] [:money \e]) 
   ($= [:more \e] [:money \e]) 
   ;; letter o
   ($= [:more \o] [:money \o]) 
   ;; Letter m
   ($= [:more \m] [:money \m])
   ;; Letter n
   ($= [:send \n] [:money \n])])

(def carries
  (for [i (range 4)]
    ($in [:_c i] 0 1)))

(def ^{:doc "Naive model, following Lecture CP_2. Sum represented with carries:
      c3 c2 c1 c0     (for all, least significant is 0)
         S  E  N  D + (indexes 3 to 0)
         M  O  R  E = (indexes 3 to 0)
      -------------
      M  O  N  E  Y   (indexes 4 to 0)"} 
  smm-naive-model
  (concat (costraint-0-9 send)
          (costraint-0-9 more)
          (costraint-0-9 money)
          letter-ties
          carries [($distinct send)
                   ($distinct more)
                   ($distinct money)
                   ($!= [:send \s] 0)
                   ($!= [:more \m] 0)
                   ($!= [:money \m] 0)
                   
                   ($= ($+         [:send \d] [:more \e]) ($+ ($* 10 [:_c 0]) [:money \y]))
                   ($= ($+ [:_c 0] [:send \n] [:more \r]) ($+ ($* 10 [:_c 1]) [:money \e]))
                   ($= ($+ [:_c 1] [:send \e] [:more \o]) ($+ ($* 10 [:_c 2]) [:money \n]))
                   ($= ($+ [:_c 2] [:send \s] [:more \m]) ($+ ($* 10 [:_c 3]) [:money \o]))
                   ($=     [:_c 3]                                            [:money \m])]))

(defn print-random-sol
  []
  ;; some gobbledygook to rearrange the result
  (let [apply-hm (partial apply hash-map)
        merging-f #(merge %1 (apply-hm %2))
        ssssol (rand-nth (solutions smm-naive-model))
        word-map (reduce merge
                         (for [part (partition-by ffirst (sort-by ffirst ssssol))]
                                 (let [part-map (map apply-hm (map #(let [key (first (first %1))
                                                                          letter (second (first %1))
                                                                          value (second %1)]
                                                                      (vector key [letter value])) part))
                                       merged-val-map (first (apply merge-with concat part-map))] 

                                   (hash-map (key merged-val-map) (apply-hm (val merged-val-map))))))]
    
    (let [s-map (:send word-map)
          m1-map (:more word-map)
          m2-map (:money word-map)]
      (pprint s-map)
      (print (str " " (apply str [(s-map \s) (s-map \e) (s-map \n) (s-map \d)]) " +\n"
                  " "(apply str [(m1-map \m) (m1-map \o) (m1-map \r) (m1-map \e)]) " =\n"
                  "-------\n"
                  (apply str [(m2-map \m) (m2-map \o) (m2-map \n) (m2-map \e) (m2-map \y)]) "\n"
                  )))))














































