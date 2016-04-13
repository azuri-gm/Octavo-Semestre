;----------------------------------------------------------
; Problem 61: Map Construction
; Date: March 17, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------
    
(use 'clojure.test)

(def f
  (fn 
  	[x y]
      (into {} (map 
      	#(hash-map %1 %2) x y))))

(deftest test-f
    (is (= (f [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
    (is (= (f [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
    (is (= (f [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(run-tests)