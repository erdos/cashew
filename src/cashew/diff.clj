(ns cashew.diff
  "Symbolic differentiation"
  (:require [cashew.core :as c :refer [plus mult dispatch]]))

(defmulti diff dispatch)

(defmethod diff ::c/number [_ _] 0)
(defmethod diff ::c/symbol [x v]
  (if (= x v) 1 0))

(defmethod diff 'pow [[_ x] variable]
  nil ;; TODOO
  )

;; chain rule
(doseq [[f df] {::c/sin #(list 'cos %)
                ::c/cos #(list '* -1 (list 'sin %))
                ::c/exp #(list 'exp %)
                ::c/log #(list 'pow % -1)
                ::c/sqrt #(list '* 1/2 (list 'pow % -1/2))}]
  (defmethod diff f [[_ g] variable]
    (mult (df g) (diff g variable))))

(defmethod diff ::c/pow [[_ b e] variable]
  (if (or (number? e) (and (symbol? e) (not= e variable)))
    (mult e (list 'pow b (plus e -1)))
    (throw (ex-info "Symbolic differentiation of form is not supported" {:exponent e}))))

;; returns lazy seq of triples of [before elem after] for each elem in the collection
(defn- go-over [xs]
  (when (seq xs)
    ((fn f [before x after]
       (cons [before x after]
             (when (seq after)
               (lazy-seq (f (cons x before) (first after) (next after))))))
     () (first xs) (next xs))))

(defmethod diff ::c/* [[_ & xs] variable]
  (apply plus
         (for [[as b bs] (go-over xs)]
           (apply mult (diff b variable) (concat as bs)))))

(defmethod diff ::c/+ [[_ & xs] variable]
  (apply plus (map #(diff % variable) xs)))

(defmethod diff ::c/- [[_ & xs] variable]
  (cons '- (map #(diff % variable) xs)))

;;; TODO: division, nth root, etc.
