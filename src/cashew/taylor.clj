(ns cashew.taylor
  "Taylor-series for expressions"
  (:require [cashew.core :as c :refer [factorial pow mult plus plus' dispatch]]
            [cashew.diff :refer [diff]]))

(deftype TaylorSeries [limit terms-seq]
  clojure.lang.Seqable
  (seq [_] (list* '+ terms-seq))
  Object
  (toString [_] (str "<Taylor series of " limit ">"))
  (equals [_ other] (= (.limit other) limit)))

(ns-unmap *ns* 'map->TaylorSeries)
(ns-unmap *ns* '->TaylorSeries)

(defn- pow-1-n [n]
  (assert (integer? n))
  (if (even? n) 1 -1))

(defmulti taylor dispatch)

(derive TaylorSeries ::c/any)

(defmethod taylor TaylorSeries [taylor-series] taylor-series)

(defmethod taylor ::c/atomic [n] n)

(defmethod taylor ::c/sin [[_ x :as expr]]
  (->> (for [n (range)
             :let [m (+ (clojure.core/* 2 n) 1)]]
         (mult (pow-1-n n)
               (pow (factorial m) -1)
               (pow x m)))
       (new TaylorSeries expr)))

(defmethod plus' [TaylorSeries TaylorSeries] [a b]
  (taylor (plus (.limit a) (.limit b))))

(defmethod plus' [TaylorSeries ::c/any] [series other]
  (taylor (plus (.limit series) other)))

(defmethod plus' [::c/any TaylorSeries] [a t] (plus t a))

#_(declare divide x)

#_
(defmethod taylor ::c/any
  ([expr] (taylor expr 'x))
  ([expr origin]
   (->> (map-indexed (fn [n df]
                       (divide (mult df (pow (- x origin) n))
                               (factorial n)))
                     (iterate diff expr))
        (new TaylorSeries expr))))

