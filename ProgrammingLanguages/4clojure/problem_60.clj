;----------------------------------------------------------
; Problem 60: Sequence Reductions
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn ff 
    ([a l] 
     (ff a (first l) (rest l)))
    ([a l c]
     (if (empty? c)
       (list l)
       (lazy-seq (cons l (ff a (a l (first c)) (rest c)) ))))))

(deftest test-f
    (is (= (take 5 (f + (range))) [0 1 3 6 10]))
    (is (= (f conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
    (is (= (last (f * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(run-tests)