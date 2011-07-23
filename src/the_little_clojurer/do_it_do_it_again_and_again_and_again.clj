(ns the-little-clojurer.do-it-do-it-again-and-again-and-again
  (:use the-little-clojurer.preface))


(def lat?                               ; define => def
  (fn [l]                               ; lambda => fn, also arguments are declared in a vector [] and not a list ()
    (cond                               ; Clojure's cond has far fewer parenthesis
     (empty? l) true                    ; #t => true, #f => false
     (atom? (first l)) (lat? (rest l))  ; No tail calls on the JVM, unlike in Scheme, this call consumes stack
     :else false)))                     ; :keywords resolve to themselves and are interned, keywords are truthy, else is just convention


(defn lat?                              ; defn is syntactic sugar for def + fn
  [l]
  (cond                                 ; Cond requires an even number of pairs
   (empty? l) true
   (atom? (first l))  (recur (rest l))  ; recur is Clojure's solution for optimized self tail-recursion
   :else false))                        ; If no clauses match, cond returns nil


(defn member?
  [a lat]
  (cond
   (empty? lat) false
   :else (or (= (first lat) a)
             (recur a (rest lat)))))  ; or is a macro and expands at compile time, doesn't look like a tail call but it is