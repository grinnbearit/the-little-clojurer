(ns the-little-clojurer.what-is-the-value-of-all-this
  (:use [the-little-clojurer [friends-and-relations :exclude [set?]]
         [preface]]))


(def new-entry build)


(defn lookup-in-entry-help
  [name names values entry-f]
  (cond
   (empty? names)
   (entry-f name)
   
   (= name (first names))
   (first values)

   :else
   (recur name
          (rest names)
          (rest values)
          entry-f)))


(defn lookup-in-entry
  [name entry entry-f]
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))


(def extend-table conj)


(defn lookup-in-table
  [name table table-f]
  (cond
   (empty? table)
   (table-f name)

   :else
   (lookup-in-entry name
                    (first table)
                    (fn [name]
                      (lookup-in-table name
                                       (rest table)
                                       table-f)))))


(declare meaning)


(defn *const
  [e table]
  (cond
   (number? e)
   e

   (= e :t)
   true

   (= e :f)
   false

   :else
   (build 'primitive e)))


(def text-of second)


(defn *quote
  [e table]
  (text-of e))


(defn initial-table
  [name]
  (throw (Exception. "Value not found")))  ; custom exceptions


(defn *identifier
  [e table]
  (lookup-in-table e table initial-table))


(defn *lambda
  [e table]
  (build 'non-primitive
         (conj (rest e) table)))


(def table-of first)


(def formals-of second)


(def body-of get-third)


(defn else?
  [x]
  (cond
   (atom? x)
   (= x 'else)

   :else
   false))


(def question-of first)


(def answer-of second)


(defn evcon
  [lines table]
  (cond
   (else? (question-of (first lines)))
   (meaning (answer-of (first lines)) table)

   (meaning (question-of (first lines)) table)
   (meaning (answer-of (first lines) table))

   :else
   (recur (rest lines) table)))


(def cond-lines-of rest)


(defn *cond
  [e table]
  (evcon (cond-lines-of e) table))


(defn evlis
  [args table]
  (cond
   (empty? args)
   ()

   :else
   (conj (evlis (rest args) table)
         (meaning (first args) table))))


(def function-of first)


(def arguments-of rest)


(defn primitive?
  [l]
  (= (first l) 'primitive))


(defn non-primitive?
  [l]
  (= (first l) 'non-primitive))


(defn atom?*
  [x]
  (cond
   (atom? x)
   true

   (empty? x)
   false

   (= (first x) 'primitive)
   true

   (= (first x) 'non-primitive)
   true

   :else
   false))


(defn apply-primitive
  [name vals]
  (cond
   (= name 'cons)
   (conj (second vals) (first vals))

   (= name 'car)
   (first (first vals))

   (= name 'cdr)
   (rest (first vals))

   (= name 'null?)
   (empty? (first vals))

   (= name 'eq?)
   (= (first vals) (second vals))

   (= name 'atom?)
   (atom?* (first vals))

   (= name 'zero?)
   (zero? (first vals))

   (= name 'add1)
   (inc (first vals))

   (= name 'sub1)
   (dec (first vals))

   (= name 'number?)
   (number? (first vals))))


(defn apply-closure
  [closure vals]
  (meaning (body-of closure)
           (extend-table (table-of closure)
                         (new-entry (formals-of closure)
                                    vals))))


(defn apply-to
  [fun vals]
  (cond
   (primitive? fun)
   (apply-primitive (second fun) vals)

   (non-primitive? fun)
   (apply-closure (second fun) vals)))


(defn *application
  [e table]
  (apply-to
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))


(defn atom-to-action
  [e]
  (cond
   (number? e)
   *const

   (= e :f)
   *const
   
   (= e :t)
   *const

   (= e 'cons)
   *const

   (= e 'car)
   *const

   (= e 'cdr)
   *const

   (= e 'null?)
   *const

   (= e 'eq?)
   *const

   (= e 'atom?)
   *const

   (= e 'zero?)
   *const

   (= e 'add1)
   *const

   (= e 'sub1)
   *const

   (= e 'number?)
   *const

   :else
   *identifier))


(defn list-to-action
  [e]
  (cond
   (atom? (first e))
   (cond
    (= (first e) 'quote)
    *quote

    (= (first e) 'lambda)
    *lambda

    (= (first e) 'cond)
    *cond

    :else
    *application)))


(defn expression-to-action
  [e]
  (cond
   (atom? e)
   (atom-to-action e)

   :else
   (list-to-action e)))


(defn value
  [e]
  (meaning e ()))


(defn meaning
  [e table]
  ((expression-to-action e) e table))
