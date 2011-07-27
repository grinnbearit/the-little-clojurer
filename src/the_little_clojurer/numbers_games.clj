(ns numbers-games
  (:refer-clojure :exclude [+ - * / > <])   ; prevents these functions from being imported
  (:use the-little-clojurer.preface))


;;; add1 => inc
;;; sub1 => dec


(defn +
  [n m]
  (cond
   (zero? n) m
   :else (recur (dec n) (inc m))))      ; a tail recursive solution


(defn -
  [n m]
  (cond
   (zero? m) n
   :else (recur (dec n) (dec m))))      ; a tail recursive solution


(defn addtup
  [tup]
  (cond
   (empty? tup) 0
   :else (+ (first tup)
            (addtup (rest tup)))))


(defn *
  [n m]
  (cond
   (zero? m) 0
   :else (+ n (* n (dec m)))))


(defn tup+
  [tup1 tup2]
  (cond
   (empty? tup1) tup2
   (empty? tup2) tup1
   :else (conj (tup+ (rest tup1) (rest tup2))
               (+ (first tup1) (first tup2)))))


(defn >
  [n m]
  (cond
   (zero? n) false
   (zero? m) true
   :else (recur (dec n) (dec m))))


(defn <
  [n m]
  (cond
   (zero? m) false
   (zero? n) true
   :else (recur (dec n) (dec m))))


(defn eq?                               ; = is required later so can't not be imported
  [n m]
  (cond
   (or (< n m) (> n m)) false
   :else true))



(defn expt                              ; ^ is special in Clojure
  [n m]
  (cond
   (zero? m) 1
   :else (* n (expt n (dec m)))))


(defn /
  [n m]
  (cond
   (< n m) 0
   :else (inc (/ (- n m) m))))


(defn length
  [lat]
  (cond
   (empty? lat) 0
   :else (inc (length (rest lat)))))


(defn pick
  [n lat]
  (cond
   (zero? (dec n)) (first lat)
   :else (recur (dec n) (rest lat))))


(defn rempick
  [n lat]
  (cond
   (zero? (dec n)) (rest lat)
   :else (conj (rempick (dec n) (rest lat))
               (first lat))))


(defn no-nums
  [lat]
  (cond
   (empty? lat) ()
   (number? (first lat)) (recur (rest lat))
   :else (conj (no-nums (rest lat))
               (first lat))))


(defn all-nums
  [lat]
  (cond
   (empty? lat) ()
   (number? (first lat)) (conj (all-nums (rest lat))
                               (first lat))
   :else (recur (rest lat))))


;;; eqan? is not necessary in Clojure, = takes care of both cases


(defn occur
  [a lat]
  (cond
   (empty? lat) 0
   (= (first lat) a) (inc (occur a (rest lat)))
   :else (recur a (rest lat))))


(defn one?
  [n]
  (= 1 n))


(defn rempick
  [n lat]
  (cond
   (one? n) (rest lat)
   :else (conj (rempick (dec n) (rest lat))
               (first lat))))