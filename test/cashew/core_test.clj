(ns cashew.core-test
  (:require [clojure.test :refer :all]
            [cashew.core :refer :all]
            [cashew.diff :refer :all]
            [cashew.taylor :refer :all]))


(deftest test-mult
  (is (= '(* a b c) (mult 'a 'b 'c)))
  (testing "Zero"
    (is (= 0 (mult 'a 0 'b 'c)))
    (is (= 0 (mult 0 1)))
    (is (= 0 (mult 0 'a))))
  (testing "Eager evaluation"
    (is (= 6 (mult 2 3)))
    (is (= '(* 6 a) (mult 2 'a 3)))
    (is (= '(* 6 a) (mult 'a 2 3)))
    (is (= '(* 6 a) (mult 2 3 'a))))
  (testing "Unit"
    (is (= '(* a b) (mult 'a 1 'b)))
    (is (= 'a (mult 'a 1))))
  (testing "Merging factors"
    (is (= '(* a b c d) (mult (mult 'a 'b) (mult 'c 'd))))
    (is (= '(* a b c) (mult 'a '(* b c)))))
  #_(testing "Simplify to pow"
      (is (= '(pow a 2) (mult 'a 'a)))
      (is (= '(pow a 3) (mult 'a 'a 'a)))))


(deftest test-pow
  (is (= 1 (pow 'x 0)))
  (is (= 0 (pow 0 (mult 'a 'b)))))


(deftest test-plus
  (is (= '(+ a b 1 2 c) (plus 'a 'b 1 2 'c)))
  (is (= '123 (plus 123)))
  (testing "Eager evaluation"
    (is (= 2 (plus 1 1))))
  (testing "zero"
    (is (= 'x (plus 'x 0)))
    (is (= 0 (plus 0 0))))
  (testing "Nested"
    (is (= '(+ a b c d) (plus (plus 'a 'b) (plus 'c 'd))))))


(deftest test-reorder
  (testing "Atomic cases"
    (is (= 3 (reorder 3)))
    (is (= 'x (reorder 'x))))
  (testing "Complex case"
    (is (= '(* 3 x y (+ 2 3))
           (reorder '(* (+ 3 2) x 3 y))))))

(deftest test-expand
  (is (= 'a (expand 'a)))
  (is (= '(+ a b c) (expand '(+ a b c))))
  (is (= '(* a b c) (expand '(* a b c))))
  (is (= '(+ (* 3 a b c) (* 2 a b c))
         (expand '(* a b c (+ 3 (* 1 2)))))))


(deftest test-canonize
  (testing "Addition"
    (is (= '(+ a b c) (canonize '(+ a b c))))
    (is (= '(* 4 a) (canonize '(+ a (* 3 a)))))
    (is (= '(+ (* 4 a) b) (canonize '(+ a (* 2 a) b a))))
    (is (= '0 (canonize '(+ a (* -1 a)))))
    (is (= 'b (canonize '(+ a b (* -1 a)))))))


(deftest test-taylor
  (doseq [i [0 1 2]]
    (is (= i (taylor i))))

  (is (= (taylor '(sin x))
         (taylor (taylor '(sin x))))))

:ok


