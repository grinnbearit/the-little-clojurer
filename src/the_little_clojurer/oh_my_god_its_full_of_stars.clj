(ns oh-my-god-its-full-of-stars
  (:use the-little-clojurer.preface))


(defn rember*
  [a l]
  (cond
   (empty? l)
   ()

   (atom? (first l))
   (cond
    (= (first l) a)
    (recur a (rest l))

    :else
    (conj (rember* a (rest l))
          (first l)))

   :else
   (conj (rember* a (rest l))
         (rember* a (first l)))))


(defn insertR*
  [new old l]
  (cond
   (empty? l)
   ()

   (atom? (first l))
   (cond
    (= old (first l))
    (conj (insertR* new old (rest l))
          new
          old)

    :else
    (conj (insertR* new old (rest l))
          (first l)))

   :else
   (conj (insertR* new old (rest l))
         (insertR* new old (first l)))))


(defn occur*
  [a l]
  (cond
   (empty? l)
   0

   (atom? (first l))
   (cond
    (= a (first l))
    (inc (occur* a (rest l)))

    :else
    (recur a (rest l)))

   :else
   (+ (occur* a (rest l))
      (occur* a (first l)))))


(defn subst*
  [new old l]
  (cond
   (empty? l)
   ()

   (atom? (first l))
   (cond
    (= old (first l))
    (conj (subst* new old (rest l))
          new)

    :else
    (conj (subst* new old (rest l))
          (first l)))

   :else
   (conj (subst* new old (rest l))
         (subst* new old (first l)))))


(defn insertL*
  [new old l]
  (cond
   (empty? l)
   ()

   (atom? (first l))
   (cond
    (= old (first l))
    (conj (insertL* new old (rest l))
          old
          new)

    :else
    (conj (insertL* new old (rest l))
          (first l)))

   :else
   (conj (insertL* new old (rest l))
         (insertL* new old (first l)))))


(defn member*
  [a l]
  (cond
   (empty? l)
   false

   (atom? (first l))
   (cond
    (= a (first l))
    true

    :else
    (recur a (rest l)))

   :else
   (or (member* a (rest l))
       (recur a (first l)))))


(defn leftmost
  [l]
  (cond
   (atom? (first l))
   (first l)

   :else
   (recur (rest l))))


(defn eqlist?
  [l1 l2]
  (cond
   (and (empty? l1)
        (empty? l2))
   true

   (or (empty? l1)
       (empty? l2))
   false

   (and (atom? (first l1))
        (atom? (first l2)))
   (and (= (first l1)
           (first l2))
        (recur (rest l1)
               (rest l2)))

   (or (atom? (first l1))
       (atom? (first l2)))
   false

   :else
   (and (eqlist? (rest l1) (rest l2))
        (recur (first l1) (first l2)))))


(defn equal?
  [s1 s2]
  (cond
   (and (atom? s1)
        (atom? s2))
   (= s1 s2)

   (or (atom? s1)
       (atom? s2))
   false

   :else
   (eqlist? s1 s2)))


(defn eqlist?
  [l1 l2]
  (cond
   (and (empty? l1)
        (empty? l2))
   true

   (or (empty? l1)
       (empty? l2))
   false

   :else
   (and (equal? (first l1)
                (first l2))
        (eqlist? (rest l1)
                 (rest l2)))))


(defn rember
  [s l]
  (cond
   (empty? l)
   ()

   (= (first l) s)
   (rest l)

   :else
   (conj (rember s (rest l))
         s)))