(ns cashew.poly-test
  (:require [clojure.test :refer :all]
            [cashew.core :refer :all]
            [cashew.poly :refer :all]))


(deftest trivial
  (testing "Trivial polynomials"
    (is (= 0 (factors->polynomial [])))
    #_(is (= 6 (factors->polynomial [6])))))


(deftest test-lead
  (is (= (factors->polynomial [2 0 0])
         (lead (factors->polynomial [2 1 1])))))


(deftest polynomial-addition
  (testing "Polynomial addition"
    (is (= (factors->polynomial [2 2])
           (plus (factors->polynomial [1 1 1])
                 (factors->polynomial [-1 1 1]))))
    (is (= (factors->polynomial [1 2 2 1])
           (plus (factors->polynomial [1 1 0 1])
                 (factors->polynomial [1 2 0]))))))


(deftest polynomial-multiplication
  (testing "Polynomial multiplication"
    (is (= (factors->polynomial [1 0 -1])
           (mult (factors->polynomial [1 1])
                 (factors->polynomial [1 -1]))))))


(deftest polynomial-division-and-remainder
  (testing "Polynomial division"
    (is (= [(factors->polynomial [1 -1]) 0] ;; x - 1
           (quot&rem (factors->polynomial [1 0 -1]) ;; x^2 - 1
                     (factors->polynomial [1 1])))) ;; x + 1
    ))


;; should give a+1
#_
(deftest test-polynomial-division
  (println "------")
  (quot&rem (factors->polynomial [(mult 'a 'a) 0 -1])
            (factors->polynomial ['a -1])))