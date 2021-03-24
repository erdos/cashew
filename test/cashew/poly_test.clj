(ns cashew.poly-test
  (:require [clojure.test :refer :all]
            [cashew.core :refer :all]
            [cashew.poly :refer :all]))


(deftest trivial
  (testing "Trivial polynomials"
    (is (= 0 (coeffs->polynomial [])))
    #_(is (= 6 (coeffs->polynomial [6])))))


(deftest test-lead
  (is (= (coeffs->polynomial [2 0 0])
         (lead (coeffs->polynomial [2 1 1])))))


(deftest polynomial-addition
  (testing "Polynomial addition"
    (is (= (coeffs->polynomial [2 2])
           (plus (coeffs->polynomial [1 1 1])
                 (coeffs->polynomial [-1 1 1]))))
    (is (= (coeffs->polynomial [1 2 2 1])
           (plus (coeffs->polynomial [1 1 0 1])
                 (coeffs->polynomial [1 2 0]))))))


(deftest polynomial-multiplication
  (testing "Polynomial multiplication"
    (is (= (coeffs->polynomial [1 0 -1])
           (mult (coeffs->polynomial [1 1])
                 (coeffs->polynomial [1 -1]))))))


(deftest polynomial-division-and-remainder
  (testing "Polynomial division"
    (is (= [(coeffs->polynomial [1 -1]) 0] ;; x - 1
           (quot&rem (coeffs->polynomial [1 0 -1]) ;; x^2 - 1
                     (coeffs->polynomial [1 1])))) ;; x + 1
    
    ;; paramet
    (is (= [(coeffs->polynomial ['a +1]) 0]
           (quot&rem (coeffs->polynomial [(mult 'a 'a) 0 -1])
                     (coeffs->polynomial ['a -1]))))))

#_
(deftest failing
  (println "> "
           (quot&rem (coeffs->polynomial [(mult 'a 'b) 0 -1])
                     (coeffs->polynomial ['a -1]))))

