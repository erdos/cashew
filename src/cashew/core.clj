(ns cashew.core)

(declare cartesian plus plus? minus pow pow? mult mult?)

(def third (comp first next next))

(def factorial (memoize (fn [n] (assert (integer? n)) (reduce clojure.core/* (range 1 (inc n))))))

(def tag-dispatch (memoize (fn [s] (keyword "cashew.core" (name s)))))

(defn dispatch [x & _]
  (cond (= 0 x)     ::zero
        (= 1 x)     ::one
        (number? x) ::number
        (symbol? x) ::symbol
        (seq? x)    (tag-dispatch (first x))
        :else       (type x)))

(defn wrap-dispatch [multimethod]
  (memoize
   (fn [dispatch-val]
     (let [multimethod @(.v multimethod)
           ps (prefers multimethod)
           candidates (cartesian (map (fn [e] (cons e (ancestors e))) dispatch-val))
           candidates (filter (methods multimethod) candidates)]
       (or (if (> 1 (count candidates))
             (let [candidate-overwrites (mapcat ps candidates)
                   clear-candidates (remove
                                     (fn [c] (some (fn [p] (isa? c p)) candidate-overwrites))
                                     candidates)
                   clear-candidates (filter
                             ;; only keep candidates that have at least 1 exact value
                                     (fn [c] (some true? (map = c dispatch-val)))
                                     clear-candidates)]
               (doto (first clear-candidates)))
             (first candidates))
           dispatch-val)))))

(derive ::one ::number)
(derive ::zero ::number)

(derive ::number ::atomic)
(derive ::symbol ::atomic)

(derive ::atomic ::any)

(derive ::+ ::any)
(derive ::* ::any)
(derive ::pow ::any) ;; should go to default!

;; na ez szivas.
; (derive '* ::any)

(defn- moses [f xs] [(filter f xs) (remove f xs)])

;; addition

(defn plus? [x] (and (seq? x) (= '+ (first x))))

(defmulti canonize dispatch)

(defmethod canonize ::any [x] x)

(defmethod canonize ::+ [[_ & terms]]
  (->>
   (reduce (fn [m term]
             (cond (and (mult? term) (= 3 (count term)) (number? (second term)))
                   (update m (third term) (fnil + 0) (second term))
                   (and (mult? term) (= 3 (count term)) (number? (third term)))
                   (update m (second term) (fnil + 0) (third term))
                   :else (update m term (fnil inc 0))))
           (array-map) terms)
   (keep (fn [[term count]]
           (cond (= 1 count)    term
                 (not= 0 count) (mult count term))))
   ((fn [xs] (case (count xs)
               0 0
               1 (first xs)
               (cons '+ xs))))))

(defmethod canonize ::* [[_ & terms]]
  (->>
   (reduce (fn [m term]
             (if (and (pow? term) (= 3 (count term)) (number? (third term)))
               (update m (second term) (fnil + 0) (third term))
               (update m term (fnil inc 0))))
           (array-map) terms)
   (keep (fn [[term count]]
           (cond (= 1 count)    term
                 (not= 0 count) (pow term count))))
   ((fn [xs] (case (count xs)
               0 1
               1 (first xs)
               (cons '* xs))))))

; (prefer-method canonize ::plus ::any)

(defmulti plus' (comp (wrap-dispatch plus') (partial mapv dispatch) list))

(defn plus [x & xs] (reduce plus' x xs))

(defmethod plus' [::number ::number] [a b] (+ a b))

(defmethod plus' [::any ::zero] [x _] x)
(defmethod plus' [::zero ::any] [_ x] x)
(defmethod plus' :default [a b]
  (if (= a b)
    (mult 2 a)
    (list '+ a b)))

;; must be preferred:
(defmethod plus' [::any ::+] [a [_ & terms]]
  (list* '+ a terms))

(defmethod plus' [::+ ::any] [[_ & terms] b]
  (concat ['+] terms [b]))

(defmethod plus' [::+ ::+] [[_ & terms1] [_ & terms2]]
  (list* '+ (concat terms1 terms2)))

;; multiplication

(def mult? (comp #{::*} dispatch))

(defmulti mult' (comp (wrap-dispatch mult') (partial mapv dispatch) list))
(defn mult [x & xs] (reduce mult' x xs))

(defmethod mult' [::number ::number] [a b] (* a b))

(defmethod mult' [::any ::zero] [_ _] 0)
(defmethod mult' [::zero ::any] [_ _] 0)
(defmethod mult' [::any ::one] [x _] x)
(defmethod mult' [::one ::any] [_ x] x)

; (defmethod mult' [::any ::any] [a b] (list '* a b))
(defmethod mult' :default [a b]
  (if (= a b)
    (pow a 2)
    (list '* a b)))

(defmethod mult' [::any ::*] [a [_ & factors]] (apply mult a factors))

#_(defmethod mult' [::* ::any] [[_ & factors] b]
    (list* '* (concat factors [b])))


(defmethod mult' [::* ::any] [[_ & factors] b]
  (list* '* (concat factors [b]))
  (or
     ;; there is a power fn call with the same base around.
   #_(let [[ps others] (moses (fn [f] (and (pow? f) (= (:base f) s))) factors)]
       (when (not-empty ps)
       ;; 
         ))
   (let [[ps others] (moses (fn [f] (= f b)) factors)]
     (when (not-empty ps)
       (mult (pow b (inc (count others))) others)))
   (list* '* (concat factors [b]))))

(defmethod mult' [::* ::number] [[_ & factors] b]
  (let [[numbers factors] (moses number? factors)]
    (list* '* (reduce * b numbers) factors)))

(defmethod mult' [::* ::*] [[_ & factors1] [_ & factors2]]
  (apply mult (concat factors1 factors2)))

(prefer-method mult' [::zero ::any] [::any ::one])
(prefer-method mult' [::any ::zero] [::one ::any])

;; --- power
;; 

(def pow? (comp #{::pow} dispatch))

(defmulti pow' (comp (wrap-dispatch pow') (partial mapv dispatch) list))
(defn pow [x & xs] (reduce pow' x xs))

(defmethod pow' [::number ::number] [a b] (Math/pow a b))
(defmethod pow' [::zero ::zero] [_ _] (assert false "Not defined"))
(defmethod pow' [::any ::zero] [_ _] 1)
(defmethod pow' [::zero ::any] [_ _] 0)
(defmethod pow' :default [a b] (list 'pow a b))


;; ----

(defn minus [a b]
  (assert (some? a))
  (assert (some? b))
  (if (= a b)
    0
    (plus a (mult -1 b))))

;; ----

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn sort-score [x]
  (case (dispatch x)
    ::number [0 x]
    ::symbol [1 x]
    +        [2 x]
    -        [3 x]
    *        [4 x]
    /        [5 x]
    (exp)    [6 x]
    (sin cos tan tanh atan asin acos) [7 x]
    (pow sqrt) [8 x]
    [9 x]))

(defn reorder
  "Reorder terms in an expression  to make it easier to read."
  [x]
  ;; TODO check meta if already ordered do not reorder polynomial, taylor, etc.
  (if (seq? x)
    (list* (first x) (map reorder (sort-by sort-score (next x))))
    x))

;; belul osszeadas, kivul szorzsa
(defn factor [expression])

;; kivul osszeadas, belul szorzas, legbelul hatvanyozas
(defmulti expand dispatch)
(defmethod expand ::atomic [x] x)

#_(defmethod '+ [[_ & terms]])

(defn cartesian [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian (rest colls)), x (first colls)]
      (cons x more))))

(defmethod expand ::* [[_ & factors]]
  (let [factors (map expand factors)
        [additions multiplications] (moses plus? factors)
        ; [numbers multiplications] (moses number? multiplications)
        ]
    (apply plus (for [addition (cartesian (map next additions))]
                  (apply mult (concat multiplications addition))))))

(defmethod expand ::+ [[_ & terms]] (apply plus terms))

;; fncalls are kept as they are.

;; fncalls
(defmethod expand ::sin [x] x)
(defmethod expand ::cos [x] x)
(defmethod expand ::pow [x] x)

(defmulti substitute dispatch)

(defmethod substitute ::number [n _] n)

(defmethod substitute ::symbol [s m] (get m s s))

(defmethod substitute ::+ [[_ & xs] m]
  (apply plus (for [x xs] (substitute x m))))

(defmethod substitute ::* [[_ & xs] m]
  (apply mult (for [x xs] (substitute x m))))


;; division with remainder
;; 
(defmulti quot&rem (comp (wrap-dispatch quot&rem) (partial mapv dispatch) list))

(defmethod quot&rem [::number ::number] [a b]
  [(quot a b) (rem a b)])

(defmethod quot&rem [::any ::one] [a _]
  [a 0])

(defmethod quot&rem [::any ::zero] [_ _]
  (throw (ex-info "Division by zero!" {})))

(defmethod quot&rem [::pow ::any] [[_ base p] other]
  (if (= base other)
    (pow base (minus p 1))
    (throw (ex-info "Can not reduce unexpected power" {:pow [base p] :divisor other}))))

(defmethod quot&rem [::any ::any] [a b]
  (if (= a b)
    [1 0]
    (throw (ex-info "Unexpected division" {:a a :b b
                                           :at (dispatch a)
                                           :bt (dispatch b)}))))

;; util methods
;; 

(defn square [x] (pow x 2))
