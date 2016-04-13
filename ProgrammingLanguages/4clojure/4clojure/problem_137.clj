;----------------------------------------------------------
; Problem 137: Digits and Bases
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn [x y]
      (loop [x x l []] 
            (if (zero? x) 
              (if (empty? l) 
                [0] 
                (reverse l))
              (recur (quot x y) (conj l (mod x y)))))))

(deftest test-f
    (is (= [1 2 3 4 5 0 1] (f 1234501 10)))
    (is (= [0] (f 0 11)))
    (is (= [1 0 0 1] (f 9 2)))
    (is (= [1 0] (let [n (rand-int 100000)](f n n))))
    (is (= [16 18 5 24 15 1] (f Integer/MAX_VALUE 42))))

(run-tests)