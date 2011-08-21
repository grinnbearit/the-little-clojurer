(ns the-little-clojurer.and-again-and-again-and-again
  (:refer-clojure :exclude [shuffle])
  (:use [the-little-clojurer [numbers-games :exclude [+ - * / < >]]
         [friends-and-relations :exclude [set?]] [preface]]))


(defn keep-looking
  [a sorn lat]
  (cond
   (number? sorn)
   (recur a (pick sorn lat) lat)

   :else
   (= a sorn)))


(defn looking
  [a lat]
  (keep-looking a (pick 1 lat) lat))


(defn eternity
  [x]
  (recur x))


(defn shift
  [pair]
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))


(defn align
  [pora]
  (cond
   (atom? pora)
   pora

   (a-pair? (first pora))
   (recur (shift pora))

   :else
   (build (first pora)
          (align (second pora)))))


(defn length*
  [pora]
  (cond
   (atom? pora)
   1

   :else
   (+ (length* (first pora))
      (length* (second pora)))))


(defn weight*
  [pora]
  (cond
   (atom? pora)
   1

   :else
   (+ (* (weight* (first pora)) 2)
      (weight* (second pora)))))


(defn shuffle
  [pora]
  (cond
   (atom? pora)
   pora

   (a-pair? (first pora))
   (recur (revpair pora))

   :else
   (build (first pora)
          (shuffle (second pora)))))


(defn C
  [n]
  (cond
   (one? n)
   1

   (even? n)
   (recur (/ n 2))

   :else
   (recur (inc (* 3 n)))))


(defn A
  [n m]
  (cond
   (zero? n)
   (inc m)

   (zero? m)
   (recur (dec n) 1)

   :else
   (recur (dec n) (A n (dec m)))))


;;; length0
(fn [l]
  (cond
   (empty? l)
   0

   :else
   (eternity (rest l))))


;;; length<=1
(fn [l]
  (cond
   (empty? l)
   0

   :else
   (inc
    ((fn [l]
       (cond
        (empty? l)
        0

        :else
        (eternity (rest l))))
     (rest l)))))


;;; length<=2
(fn [l]
  (cond
   (empty? l)
   0

   :else
   (inc
    ((fn [l]
       (cond
        (empty? l)
        0

        :else
        (inc ((fn [l]
                (cond
                 (empty? l)
                 0

                 :else
                 (eternity (rest l))))
              (rest l)))))
     (rest l)))))


;;; length0
((fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l))))))
 eternity)


;;; length<=1
((fn [f]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (f (rest l))))))
 ((fn [g]
    (fn [l]
      (cond
       (empty? l)
       0

       :else
       (inc (g (rest l))))))
  eternity))


;;; length<=2
((fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l))))))
 ((fn [length]
    (fn [l]
      (cond
       (empty? l)
       0

       :else
       (inc (length (rest l))))))
  ((fn [length]
     (fn [l]
       (cond
        (empty? l)
        0

        :else
        (inc (length (rest l))))))
   eternity)))


;;; length0
((fn [mk-length]
   (mk-length eternity))
 (fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l)))))))


;;; length<=1
((fn [mk-length]
   (mk-length
    (mk-length eternity)))
 (fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l)))))))


;;; length<=2
((fn [mk-length]
   (mk-length
    (mk-length
     (mk-length eternity))))
 (fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l)))))))


;;; length<=3
((fn [mk-length]
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l)))))))


;;; length0
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (mk-length (rest l)))))))


;;; length<=1
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc ((mk-length eternity) (rest l)))))))


;;; length
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc ((mk-length mk-length) (rest l)))))))


;;; length (stack overflow)
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   ((fn [length]
      (fn [l]
        (cond
         (empty? l)
         0

         :else
         (inc (length (rest l))))))
    (mk-length mk-length))))            ; because this is evaluated


;;; length
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc ((fn [x]
              ((mk-length mk-length) x)) (rest l)))))))


;;; length, some abstractiion
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   ((fn [length]
      (fn [l]
        (cond
         (empty? l)
         0

         :else
         (inc (length (rest l))))))
    (fn [x]
      ((mk-length mk-length) x)))))


;;; length, even more abstraction
((fn [mk-length]
   (mk-length mk-length))
 (fn [mk-length]
   ((fn [le]
      (le (fn [x]
            ((mk-length mk-length) x))))
    (fn [length]
      (fn [l]
        (cond
         (empty? l)
         0

         :else
         (inc (length (rest l)))))))))


;;; Still length, but moved the fn which "looks like" length to the top
((fn [le]
   ((fn [mk-length]
      (mk-length mk-length))
    (fn [mk-length]
      (le (fn [x]
            ((mk-length mk-length) x))))))
 (fn [length]
   (fn [l]
     (cond
      (empty? l)
      0

      :else
      (inc (length (rest l)))))))


;;; applicative-order Y combinator
(defn Y
  [le]
  ((fn [f]
     (f f))
   (fn [f]
     (le (fn [x]
           ((f f) x))))))