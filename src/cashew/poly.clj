(ns cashew.poly
  (:require [cashew.core :as c :refer :all]))

(declare coeffs->polynomial lead divide)

(deftype Polynomial [coeffs variable]
  clojure.lang.Seqable
  (seq [_] nil)
  Object
  (toString [_] (str "<Polynomial of " (vec coeffs) " >"))
  (equals [_ other] (and (instance? Polynomial other)
                         (= (.coeffs ^Polynomial other) coeffs))))

(defn poly? [x] (instance? Polynomial x))

(defn coefficients
  "Returns the vector of coefficients of the polynomial."
  [^Polynomial polynomial]
  (assert (poly? polynomial))
  (.coeffs polynomial))

(derive Polynomial ::c/any)


(ns-unmap *ns* 'map->Polynomial)
(ns-unmap *ns* '->Polynomial)

(defn degree [^Polynomial p]
  (if (instance? Polynomial p)
    (dec (count (coefficients p)))
    0))

;; leading term as polynomial
(defn lead [p]
  (coeffs->polynomial
   (cons (first (coefficients p))
         (repeat (degree p) 0))))

;; 
(defn divide-lead [p q]
  (let [[main _] (quot&rem (first (coefficients p)) (first (coefficients q)))]
    (coeffs->polynomial (cons main (repeat (- (degree p) (degree q)) 0)))))

(defmethod quot&rem [Polynomial Polynomial] [n d]
  (loop [q (identity 0) ;; needs to be unboxed for reflection warning
         r n]
    (if (and (not= r 0)
             (>= (degree r) (degree d)))
      (let [t (divide-lead (lead r) (lead d))
            q (plus q t)
            r (minus r (mult t d))]
        (recur q r))
      [q r])))

;; [3 4 1] -> 3x2 + 4x + 1
(defn coeffs->polynomial [coeffs]
  (assert (not= 0 (first coeffs)))
  (if (empty? coeffs)
    0
    (new Polynomial coeffs 'x)))

;; add them term-by-term
(defmethod plus' [Polynomial Polynomial] [^Polynomial p ^Polynomial q]
  (assert (= (.variable p) (.variable q)))
  (->> (map plus
            (concat (repeat (- (degree q) (degree p)) 0) (coefficients p))
            (concat (repeat (- (degree p) (degree q)) 0) (coefficients q)))
       (map canonize)
       (drop-while #{0})
       (coeffs->polynomial)))

(defmethod mult' [Polynomial ::c/number] [p n]
  (->> (coefficients p)
       (map (partial mult n))
       (map canonize) ;; maybe not necessary?
       (coeffs->polynomial)))

;; commutative
(defmethod mult' [::c/number Polynomial] [n p] (mult' p n))

;; factors helyett coeffs!!

(defmethod mult' [Polynomial Polynomial] [p q]
  (coeffs->polynomial
   (reduce (fn [v [idx value]] (update v idx plus value))
           (vec (repeat (+ 1 (degree p) (degree q)) 0))
           (for [[idx1 fac1] (map-indexed vector (coefficients p))
                 [idx2 fac2] (map-indexed vector (coefficients q))]
             [(+ idx1 idx2) (mult fac1 fac2)]))))

#_(defn polynomial [expression variable])

;; compose polynomials
(defn compose [outer inner] nil)

;; differencialas!
