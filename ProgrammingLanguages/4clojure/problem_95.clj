;----------------------------------------------------------
; Problem 95: To Tree, or not to Tree
; Date: March 17, 2016.
; Authors:
;           A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
    (fn ff [l]
      (cond 
        (nil? l) true
        (not (coll? l)) false
        (not (= 3 (count l))) false
        :else (and (ff (second l)) (ff (nth l 2))))))

(deftest test-f
    (is (= (f '(:a (:b nil nil) nil)) true))
    (is (= (f '(:a (:b nil nil))) false))
    (is (= (f [1 nil [2 [3 nil nil] [4 nil nil]]]) true))
    (is (= (f [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false))
    (is (= (f [1 [2 [3 [4 nil nil] nil] nil] nil]) true))
    (is (= (f [1 [2 [3 [4 false nil] nil] nil] nil]) false))
    (is (= (f '(:a nil ())) false)))

(run-tests)