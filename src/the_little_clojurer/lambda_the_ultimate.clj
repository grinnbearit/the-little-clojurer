(ns the-little-clojurer.lambda-the-ultimate
  (:use
   [the-little-clojurer.preface]))


(defn rember-f
  [test? a l]
  (cond
   (empty? l)
   ()

   (test? a (first l))
   (recur test? a (rest l))

   :else
   (conj (rember-f test? a l)
         (first l))))


(defn eq-c?
  [a]
  (fn [x]                               ; lambda => fn
    (= a x)))


(def eq-salad?
  (eq-c? :salad))                       ; keywords instead of symbols


(defn rember-f
  [test?]
  (fn [a l]
    (cond
     (empty? l)
     ()

     (test? a (first l))
     (recur a (rest l))                 ; recur uses the test? trapped in the closure

     :else
     (conj ((rember-f test?) a (rest l))
           a))))


(defn rember-f
  [test?]
  (fn rember-test? [a l]                ; named anonymous functions just for such an emergency,
    (cond
     (empty? l)
     ()

     (test? a (first l))
     (recur a (rest l))                 ; recur uses the test? trapped in the closure

     :else
     (conj (rember-test? a (rest l))    ; rember-test? only bound in body
           a))))


(def rember-=
  (rember-f =))


(defn insertL-f
  [test?]
  (fn [new old l]
    (cond
     (empty? l)
     ()

     (test? old (first l))
     (conj l new)

     :else
     (recur new old (rest l)))))


(defn insertR-f
  [test?]
  (fn [new old l]
    (cond
     (empty? l)
     ()

     (test? old (first l))
     (conj (rest l) old new)

     :else
     (recur new old (rest l)))))


(defn seqL
  [new old l]
  (conj l old new))


(defn seqR
  [new old l]
  (conj l new old))


(defn insert-g
  [seq-g]
  (fn [new old l]
    (cond
     (empty? l)
     ()

     (= old (first l))
     (seq-g new old (rest l))

     :else
     (recur new old (rest l)))))


(def insertL
  (insert-g seqL))


(def insertR
  (insert-g seqR))


(def insertL
  (insert-g (fn [new old l]
              (conj l old new))))


(def insertR
  (insert-g (fn [new old l]
              (conj l new old))))


(defn seqS
  [new old l]
  (conj l new))


(def subst
  (insert-g seqS))


(defn atom-to-function
  [x]
  (cond
   (= x '+)
   +

   (= x '*)
   *

   :else
   #(Math/pow %1 %2)))


(defn operator
  [nexp]
  (first nexp))


(defn first-sub-exp
  [nexp]
  (first (rest nexp)))


(defn second-sub-exp
  [nexp]
  (first (rest (rest nexp))))


(defn value
  [nexp]
  (cond
   (atom? nexp)
   nexp

   :else
   ((atom-to-function (operator nexp))
    (value (first-sub-exp nexp))
    (value (second-sub-exp nexp)))))


(defn multirember-f
  [test?]
  (fn multirember-test? [a lat]
    (cond
     (empty? lat)
     ()

     (test? a (first lat))
     (recur a (rest lat))

     :else
     (conj (multirember-test? a (rest lat))
           (first a)))))


(def eq-tuna?
  (eq-c? :tuna))


(defn multiremberT
  [test?]
  (fn multirember-test? [lat]
    (cond
     (empty? lat)
     ()

     (test? (first lat))
     (recur (rest lat))

     :else
     (conj (multirember-test? (rest lat))
           (first lat)))))


(defn multirember&co
  [a lat col]
  (cond
   (empty? lat)
   (col () ())

   (= a (first lat))
   (recur a (rest lat) (fn [newlat seen]
                         (col newlat
                              (conj seen (first lat)))))

   :else
   (recur a (rest lat) (fn [newlat seen]
                         (col (conj newlat (first lat))
                              seen)))))


(defn a-friend
  [x y]
  (empty? y))


(defn multiinsertLR
  [new oldL oldR lat]
  (cond
   (empty? lat)
   ()

   (= (first lat) oldL)
   (conj (multiinsertLR new oldL oldR (rest lat))
         oldL
         new)

   (= (first lat) oldR)
   (conj (multiinsertLR new oldL oldR (rest lat))
         new
         oldR)

   :else
   (conj (multiinsertLR new oldL oldR (rest lat))
         (first lat))))


(defn multiinsertLR&co
  [new oldL oldR lat col]
  (cond
   (empty? lat)
   (col () 0 0)

   (= (first lat) oldL)
   (recur new oldL oldR (rest lat) (fn [newlat L R]
                                     (col (conj newlat oldL new) (inc L) R)))

   (= (first lat) oldR)
   (recur new oldL oldR (rest lat) (fn [newlat L R]
                                     (col (conj newlat new oldR) L (inc R))))

   :else
   (recur new oldL oldR (rest lat) (fn [newlat L R]
                                     (col (conj newlat (first lat)) L R)))))


(defn evens-only*
  [l]
  (cond
   (empty? l)
   ()

   (atom? (first l))
   (cond
    (even? (first l))
    (conj (evens-only* (rest l))
          (first l))

    :else
    (recur (rest l)))

   :else
   (conj (evens-only* (rest l))
         (evens-only* (first l)))))


(defn evens-only*&co
  [l col]
  (cond
   (empty? l)
   (col () 1 0)

   (atom? (first l))
   (cond
    (even? (first l))
    (recur (rest l) (fn [newl p s]
                      (col
                       (conj newl (first l))
                       (* p (first l))
                       s)))

    :else
    (recur (rest l) (fn [newl p s]
                      (col
                       newl
                       p
                       (+ s (first l))))))

   :else
   (recur (rest l) (fn [newl p s]
                     (evens-only*&co (first l) (fn [newnewl newp news]
                                                 (col
                                                  (conj newl newnewl)
                                                  (* p newp)
                                                  (+ s news))))))))