(ns learn-ml.concept-learning)

(defn i-identical [hypo fact]
  (cond
   (= hypo :?) true
   (= hypo :phi) false
   (= hypo fact) true
   :else false))

(defn identical [hypo fact]
  (every? true?
          (map i-identical
               hypo
               fact)))

(defn i-generalize [hypo fact]
  (cond
   (= hypo :?) :?
   (= hypo :phi) fact
   (= hypo fact) hypo
   (= fact :?) :?
   (= fact :phi) hypo
   :else :?))

(defn generalize [hypo fact]
  (map i-generalize
       hypo
       fact))

;;find-s algorithm in the book
(defn find-s [pro_facts]
  (let [init-fact (repeat (count (first pro_facts)) :phi)]
    (reduce generalize init-fact pro_facts)))

;; hypo1 > hypo2
(defn i-generalizer-than [hypo1 hypo2]
  (case hypo2
    :phi (not= hypo1 :phi)
    :? false
    (= hypo1 :?)))

;; hypo1 >= hypo2
(defn i-generalizer-or-equal-than [hypo1 hypo2]
  (or (i-generalizer-than hypo1 hypo2)
      (= hypo1 hypo2)))

;; hypo1[] >= hypo2[]
(defn generalizer-or-equal-than [hypo1 hypo2]
   (every? true?
         (map i-generalizer-or-equal-than
              hypo1
              hypo2)))


;; hypo1[] > hypo2[]
(defn generalizer-than [hypo1 hypo2]
  (and
   (some true?
         (map i-generalizer-than
              hypo1
              hypo2))
   (generalizer-or-equal-than hypo1 hypo2)))

;; hypo1 < hypo2
(defn i-specializer-than [hypo1 hypo2]
  (case hypo2
    :? (not= hypo1 :?)
    :phi false
    (= hypo1 :phi)))

;; hypo1[] < hypo2[]
(defn specializer-than [hypo1 hypo2]
  (and
   (some true?
         (map i-specializer-than
              hypo1
              hypo2))
   (generalizer-or-equal-than hypo2 hypo1)))




