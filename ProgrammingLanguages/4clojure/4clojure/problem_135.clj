;----------------------------------------------------------
; Problem 135: Infix Calculator
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn [& l]
    (loop [l l]
      (cond 
        (= 3 (count l)) ((second l) (first l) (->> l rest second ))
        :else (recur (conj (->> l rest rest rest) ((second l) (first l) (->> l rest second))))))))
  
(deftest test-f
    (is (= 7  (f 2 + 5)))
    (is (= 42 (f 38 + 48 - 2 / 2)))
    (is (= 8  (f 10 / 2 - 1 * 2)))
    (is (= 72 (f 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(run-tests)