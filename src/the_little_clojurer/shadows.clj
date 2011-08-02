(ns the-little-clojurer.shadows
  (:use the-little-clojurer.preface))


(defn numbered?
  [aexp]
  (cond

   (atom? aexp)
   (number? aexp)

   :else
   (and (numbered? (first aexp))
        (numbered? (first (rest (rest aexp)))))))


(defn value
  [nexp]

  (cond

   (atom? nexp)
   nexp

   (= '+
      (first (rest nexp)))
   (+ (value (first nexp))
      (value (first (rest (rest nexp)))))

   (= '*
      (first (rest nexp)))
   (* (value (first nexp))
      (value (first (rest (rest nexp)))))

   (= '**
      (first (rest nexp)))
   (Math/pow (value (first nexp))
             (value (first (rest (rest nexp)))))))


(defn value
  [nexp]

  (cond

   (atom? nexp)
   nexp

   (= '+
      (first nexp))
   (+ (value (first (rest nexp)))
      (value (first (rest (rest nexp)))))

   (= '*
      (first nexp))
   (* (value (first (rest nexp)))
      (value (first (rest (rest nexp)))))

   (= '**
      (first nexp))
   (Math/pow (value (first (rest nexp)))
             (value (first (rest (rest nexp)))))))


(defn first-sub-exp                     ; clojure vars can't start with a number
  [aexp]
  (first (rest aexp)))


(defn second-sub-exp
  [aexp]
  (first (rest (rest aexp))))


(defn operator
  [aexp]
  (first aexp))


(defn value
  [nexp]

  (cond

   (atom? nexp)
   nexp

   (= '+
      (operator nexp))
   (+ (value (first-sub-exp nexp))
      (value (second-sub-exp nexp)))

   (= '*
      (operator nexp))
   (* (value (first-sub-exp nexp))
      (value (second-sub-exp nexp)))

   (= '**
      (operator nexp))
   (Math/pow (value (first-sub-exp nexp))
             (value (second-sub-exp nexp)))))


(defn first-sub-exp
  [aexp]
  (first aexp))


(defn operator
  [aexp]
  (first (rest aexp)))


(defn sero?
  [n]
  (empty? n))


(defn edd1
  [n]
  (conj n ()))


(defn zub1
  [n]
  (rest n))


(defn add
  [n m]
  (cond
   (sero? n)
   m

   :else
   (recur (zub1 n) (edd1 m))))