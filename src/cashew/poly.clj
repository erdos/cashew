(ns cashew.poly
  (:require [cashew.core :as c :refer :all]))

(declare coeffs->polynomial lead divide)

(deftype Polynomial [factors variable]
  clojure.lang.Seqable
  (seq [_] nil)
  Object
  (toString [this] (str "<Polynomial of " (vec factors) " >"))
  (equals [_ other] (and (instance? Polynomial other)
                         (= (.factors other) factors))))

(defn poly? [x] (instance? Polynomial x))

(derive Polynomial ::c/any)


(ns-unmap *ns* 'map->Polynomial)
(ns-unmap *ns* '->Polynomial)

(defn degree [p]
  (if (instance? Polynomial p)
    (dec (count (.factors p)))
    (do (println "Degree of" p)
        0)))

;; leading term. should give back variable as well
(defn lead [p]
  (coeffs->polynomial
   (cons (first (.factors p))
         (repeat (degree p) 0)))
  #_(apply mult
           (first (.factors p))
           (repeat (degree p) (.variable p))))

;; 
(defn divide-lead [p q]
  (let [[main _] (quot&rem (first (.factors p)) (first (.factors q)))]
    (coeffs->polynomial (cons main (repeat (- (degree p) (degree q)) 0)))))

(defmethod quot&rem [Polynomial Polynomial] [n d]
  (loop [q 0
         r n]
    (if (and (not= r 0)
             (>= (degree r) (degree d)))
      (let [t (divide-lead (lead r) (lead d))
            q (plus q t)
            r (minus r (mult t d))]
        (recur q r))
      [q r])))

;; [3 4 1] -> 3x2 + 4x + 1
(defn coeffs->polynomial [factors]
;  (assert (not-empty factors))
  (assert (not= 0 (first factors)))
  (if (empty? factors)
    0
    (new Polynomial factors 'x)))

;; add them term-by-term
(defmethod plus' [Polynomial Polynomial] [p q]
  (assert (= (.variable p) (.variable q)))
  (->> (map plus
            (concat (repeat (- (degree q) (degree p)) 0) (.factors p))
            (concat (repeat (- (degree p) (degree q)) 0) (.factors q)))
       (map canonize)
       (drop-while #{0})
       (coeffs->polynomial)))

(defmethod mult' [Polynomial ::c/number] [p n]
  (->> (.factors p)
       (map (partial mult n))
       (map canonize)
       (coeffs->polynomial)))

;; commutative
(defmethod mult' [::c/number Polynomial] [n p] (mult' p n))

;; factors helyett coeffs!!

(defmethod mult' [Polynomial Polynomial] [p q]
  (coeffs->polynomial
   (reduce (fn [v [idx value]] (update v idx plus value))
           (vec (repeat (+ 1 (degree p) (degree q)) 0))
           (for [[idx1 fac1] (map-indexed vector (.factors p))
                 [idx2 fac2] (map-indexed vector (.factors q))]
             [(+ idx1 idx2) (mult fac1 fac2)]))))

#_(defn polynomial [expression variable])

