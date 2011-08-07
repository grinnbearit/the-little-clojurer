(ns the-little-clojurer.friends-and-relations
  (:refer-clojure :exclude [set?])               ; Excludes functions to import to prevent collisions
  (:use
   [the-little-clojurer
    [preface]
    [do-it-do-it-again-and-again-and-again]
    [cons-the-magnificent]]))


(defn set?
  [lat]
  (cond
   (empty? lat)
   true

   (member? (first lat) (rest lat))
   false

   :else
   (recur (rest lat))))


(defn makeset
  [lat]
  (cond
   (empty? lat)
   ()

   (member? (first lat) (rest lat))
   (recur (rest lat))

   :else
   (conj (makeset (rest lat))
         (first lat))))


(defn makeset
  [lat]
  (cond
   (empty? lat)
   ()

   :else
   (conj
    (makeset (multirember (first lat) (rest lat)))
    (first lat))))


(defn subset?
  [set1 set2]
  (cond
   (empty? set1)
   true

   :else
   (and (member? (first set1) set2)
        (recur (rest set1) set2))))


(defn eqset?
  [set1 set2]
  (and (subset? set1 set2)
       (subset? set2 set1)))


(defn intersect?
  [set1 set2]
  (cond
   (empty? set1)
   false

   :else
   (or
    (member? (first set1) set2)
    (intersect? (rest set1) set2))))


(defn intersect
  [set1 set2]
  (cond
   (empty? set1)
   ()

   (member? (first set1) set2)
   (conj (intersect (rest set1) set2)
         (first set1))

   :else
   (recur (rest set1) set2)))


(defn union
  [set1 set2]
  (cond
   (empty? set1)
   set2

   (member? (first set1) set2)
   (recur (rest set1) set2)

   :else
   (conj (union (rest set1) set2)
         (first set1))))


(defn intersectall
  [l-set]
  (cond
   (empty? (rest l-set))
   (first l-set)

   :else
   (intersect (first l-set)
              (intersectall (rest l-set)))))


(defn a-pair?
  [x]
  (cond
   (atom? x)
   false

   (empty? x)
   false

   (empty? (rest x))
   false

   (empty? (rest (rest x)))
   true

   :else
   false))


(defn get-first                         ; car => first
  [p]
  (first p))


(defn get-second                         ; (car (cdr x)) => second
  [p]
  (second p))


(defn build
  [s1 s2]
  (conj [] s1 s2))


(defn get-third
  [l]
  (first (rest (rest l))))


(defn fun?
  [rel]
  (set? (firsts rel)))


(defn revrel
  [rel]
  (cond
   (empty? rel)
   ()

   :else
   (conj (revrel (rest rel))
         (build
          (get-second (first rel))
          (get-first (first rel))))))


(defn revpair
  [p]
  (build
   (get-second p)
   (get-first p)))


(defn revrel
  [rel]
  (cond
   (empty? rel)
   ()

   :else
   (conj (revrel (rest rel))
         (revpair (first rel)))))


(defn seconds
  [l]
  (cond
   (empty? l) ()
   :else (conj (seconds (rest l))
               (second (first l)))))


(defn fullfun?
  [fun]
  (set? (seconds fun)))


(defn one-to-one?
  [fun]
  (fun? (revrel fun)))