(ns the-little-clojurer.toys
  (:use [the-little-clojurer.preface]))


;;; car => first
;;; (car (1 2 3)) => (first (1 2 3))
;;; 1


;;; cdr => rest
;;; (cdr (1 2 3)) => (rest (1 2 3))
;;; (2 3)


;;; cons => conj
;;; (cons {atom} {list}) => (conj {seq} {atom}) where seq => (list), [vector], {map}
;;; conj on a list appends to the beginning; (conj (butter and jelly) peanut) => (peanut, butter, and, jelly)
;;; conj on a vector appends to the end; (conj [butter and jelly] peanut) => [butter, and, jelly, peanut]
;;; conj on a map associates the pair on the map; (conj {butter jelly} [peanut and]) => {peanut and, butter jelly}


;;; null? => empty?
;;; empty lists need not be quoted in Clojure
;;; (null? (quote ())) => (empty? ())
;;; true


;;; eq? => =
;;; (eq? 5 10) => (= 5 10)
;;; false
;;; Since Clojure has immutable datatypes, it compares Hash Values, therefore all things are equable