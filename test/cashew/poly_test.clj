(ns cashew.poly-test
  (:require [clojure.test :refer :all]
            [cashew.core :refer :all]
            [cashew.poly :refer :all]))


(defn- poly [& coefficients] (coeffs->polynomial (vec coefficients)))


(deftest trivial
  (testing "Trivial polynomials"
    (is (= 0 (coeffs->polynomial [])))))


(deftest test-lead
  (is (= (poly 2 0 0)
         (lead (poly 2 1 1)))))


(deftest polynomial-addition
  (testing "Polynomial addition"
    (is (= (poly 2 2)
           (plus (poly 1 1 1) (poly -1 1 1))))
    (is (= (poly 1 2 2 1)
           (plus (poly 1 1 0 1) (poly 1 2 0)))))

  (testing "Subtraction"
    (is (= 0 (minus (poly 1 2 3)
                    (poly 1 2 3))))
    (is (= (poly 1 2)
           (minus (poly 1 2 3) (poly 1 1 1))))))


(deftest polynomial-multiplication
  (testing "Polynomial multiplication"
    (is (= (poly 1 0 -1)
           (mult (poly 1 1) (poly 1 -1))))))


(deftest polynomial-division-and-remainder
  (testing "Polynomial division"
    (is (= [(poly 1 -1) 0] ;; x - 1
           (quot&rem (poly 1  0 -1) ;; x^2 - 1
                     (poly 1 1)))) ;; x + 1
    
    ;; paramet
    (is (= [(poly 'a +1) 0]
           (quot&rem (poly (mult 'a 'a) 0 -1)
                     (poly 'a -1))))))

;;  infinite loop it is.
#_
(deftest failing
  (println "> " (quot&rem (poly (mult 'a 'b) 0 -1) (poly 'a -1))))

