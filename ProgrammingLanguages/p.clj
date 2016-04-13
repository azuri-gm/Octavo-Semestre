
(use 'clojure.test)

(defn my-concat
	[l1 l2]
	(if (empty? l1)
		l2
		(cons (first l1) (my-concat (rest l1) l2))))

(defn deep-reverse
	[l]
	(cond
		(empty? l) ()
		(list? (last l)) (cons (deep-reverse (last l)) (deep-reverse (butlast l)))
		:else (cons (last l) (deep-reverse (butlast l)))
		))


(defn encode-modified
	[li]
	(loop [l (rest li)
		a (first li)
		n 1
		r ()]
		(cond
			(nil? a) (reverse r)
			(empty? l) (if (= n 1)
				(reverse (cons a r))
				(reverse (cons [n a] r)))
			(not= a (first l)) (if (= n 1)
				(recur (rest l) (first l) 1 (cons a r))
				(recur (rest l) (first l) 1 (cons [n a] r)))
			:else (recur (rest l) (first l) (inc n) r)
			)))



(defn deriv
	[f h]
	(fn [x] (/ (- (f (+ x h)) (f x)) h) )
	)



(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(n m (l k) j ((i h g) f e d) c b a) (deep-reverse '(a b c ( d e f ( g h i)) j (k l) m n))))	
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))



(run-tests)