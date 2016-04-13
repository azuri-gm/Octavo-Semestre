;----------------------------------------------------------
; Problem 63: Group a Sequence
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn [f v]
      (loop [l (map #(hash-map (f %) %) v) 
        r {}]
        (cond 
         (empty? l) r
         (not (contains? r (->> l first first first)))
         (recur (rest l) (assoc r (->> l first first first)  (->> l first first second vector )))
         :else (recur (rest l) (merge-with #(conj %1 %2) r (first l)))))))
  
(deftest test-f
    (is (= (f #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
    (is (= (f #(apply / %) [[1 2] [2 4] [4 6] [3 6]]) {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
    (is (= (f count [[1] [1 2] [3] [1 2 3] [2 3]]) {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(run-tests)