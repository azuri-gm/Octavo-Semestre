;----------------------------------------------------------
; Problem 99: Product Digits
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f 
  (fn 
    [x y]
    (->> 
      (* x y)
      (iterate #(quot % 10))
      (take-while pos?)
      (map #(mod % 10))
      reverse
      vec)))


(deftest test-f
  (is(= (f 1 1) [1]))
  (is(= (f 99 9) [8 9 1]))
  (is(= (f 999 99) [9 8 9 0 1])))

(run-tests)