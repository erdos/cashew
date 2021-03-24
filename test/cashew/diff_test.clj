(ns cashew.diff-test
  (:require [clojure.test :refer :all]
            [cashew.core :refer :all]
            [cashew.diff :refer :all]))

(deftest test-diff-1
  (is (= 2 (diff '(* x 2) 'x)))
  #_(is (= '(* (+ x 1) (exp x))
           (diff '(* x (exp x)) 'x)))

  #_(is (= :sdf
           (diff '(pow (+ x 1) 3) 'x))))