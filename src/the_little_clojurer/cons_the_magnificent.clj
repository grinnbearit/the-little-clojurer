(ns the-little-clojurer.cons-the-magnificent
  (:use the-little-clojurer.preface))


(defn rember
  [a lat]
  (cond
   (empty? lat) ()               ; empty lists don't need to be quoted
   (= a (first lat)) (rest lat)
   :else (conj (rember a (rest lat)) ; Can't use recur here, not a tail call
               (first lat))))


(defn firsts
  [l]
  (cond
   (empty? l) ()
   :else (conj (firsts (rest l))
               (first (first l)))))


(defn insertR
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj (rest lat) new old) ; Conj can take multiple arguments after the seq, this conjs new and then old
   :else (conj (insertR new old (rest lat))
               (first lat))))


(defn insertL
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj lat new)
   :else (conj (insertL new old (rest lat))
               (first lat))))


(defn subst
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj (rest lat) new)
   :else (conj (subst new old (rest lat))
               (first lat))))


(defn subst2
  [new o1 o2 lat]
  (cond
   (empty? lat) ()
   (or (= o1 (first lat))
       (= o2 (first lat))) (conj (rest lat) new)
   :else (conj (subst2 new o1 o2 (rest lat))
               (first lat))))



(defn multirember
  [a lat]
  (cond
   (empty? lat) ()
   (= a (first lat)) (recur a (rest lat)) ; Tail call position, carpe diem
   :else (conj (multirember a (rest lat))
               (first lat))))


(defn multiinsertR
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj (multiinsertR new old (rest lat))
                             new
                             old)
   :else (conj (multiinsertR new old (rest lat))
               (first lat))))


(defn multiinsertL
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj (multiinsertL new old (rest lat))
                             old
                             new)
   :else (conj (multiinsertL new old (rest lat))
               (first lat))))


(defn multisubst
  [new old lat]
  (cond
   (empty? lat) ()
   (= old (first lat)) (conj (multisubst new old (rest lat))
                             new)
   :else (conj (multisubst new old (rest lat))
               (first lat))))