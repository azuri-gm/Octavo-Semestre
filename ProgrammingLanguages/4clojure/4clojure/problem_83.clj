;----------------------------------------------------------
; Problem 83: A Half-Truth
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
(fn 
  [& l] 
  (and 
    (reduce #(or %1 %2) l)
    (not (reduce #(and %1 %2) l)))))

(deftest test-f
    (is (= false (f false false)))
    (is (= true (f true false)))
    (is (= false (f true)))
    (is (= true (f false true false)))
    (is (= false (f true true true)))
    (is (= true (f true true true false))))

(run-tests)