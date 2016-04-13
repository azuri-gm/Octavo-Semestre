;----------------------------------------------------------
; Problem 118: Re-implement Map
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn ff [f l]
     (if (empty? l) 
     	()
     (lazy-seq 
     	(cons (f (first l)) (ff f (rest l)))))))

(deftest test-f
   (is(= [3 4 5 6 7] (f inc [2 3 4 5 6])))
   (is (= (repeat 10 nil) (f (fn [f] nil) (range 10))))
   (is(= [1000000 1000001] (->> (f inc (range)) (drop (dec 1000000)) (take 2)))))

(run-tests)