;----------------------------------------------------------
; Problem 62: Re-implement iterate
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn ff 
  	[f x]
      (lazy-seq 
      	(cons x (ff f (f x))))))

(deftest test-f
    (is (= (take 5 (f #(* 2 %) 1)) [1 2 4 8 16]))
    (is (= (take 100 (f inc 0)) (take 100 (range))))
    (is (= (take 9 (f #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(run-tests)