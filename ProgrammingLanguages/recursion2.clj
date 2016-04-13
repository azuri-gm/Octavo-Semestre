;----------------------------------------------------------
; Activity: Recursive Functions, Part II
; Date: February 4, 2016.
; Authors:
;          A01165749 Brian Flores Gonzalez
;----------------------------------------------------------

(use 'clojure.test)

; 1
(defn my-repeat
	"Takes a number n and any data x as its arguments. It returns a list that contains n copies of x"
	[x l]
	(if (zero? x)
		()
		(cons l (my-repeat (dec x) l))))

; 2
(defn invert-pairs
	"Takes as an argument a list of vectors containing two elements each. It returns a new list with every vector pair inverted."
	[l]
	(if (empty? l)
		()
		(cons (#(identity [(last %) (first %)]) (first l)) (invert-pairs (rest l)))))

; 3 
(defn enlist
	"Surrounds in a list every upper-level element of the list it takes as input"
	[l]
	(if (empty? l)
		()
		(cons (cons (first l) ()) (enlist (rest l)))))

; 4
(defn my-interleave
	"Takes two arguments: the lists a and b. It returns a list containing the first element of a, followed by the first element of b, followed by the second element of a, followed by the second element of b, and so on"
	[l1 l2]
	(if (or (empty? l1) (empty? l2))
		()
		(concat (cons (first l1) (cons (first l2) ())) (my-interleave (rest l1) (rest l2)))))

; 5
(defn my-flatten
	"Removes all the interior parenthesis of the list it takes as input."
	[l]
	(cond
		(empty? l) ()
		(list? (first l)) (concat (my-flatten (first l)) (my-flatten (rest l)))
		:else (cons (first l) (my-flatten (rest l)))))

; 6
(defn exchange
	"Takes three arguments: two non-list values x1 and x2, and a list lst. It returns a list with the same elements as lst, except that all occurrences of x1 are replaced by x2 and vice versa"
	[x y l]
	(cond
		(empty? l) ()
		(= x (first l)) (cons y (exchange x y (rest l)))
		(= y (first l)) (cons x (exchange x y (rest l)))
		:else (cons (first l) (exchange x y (rest l)))))

; 7
(defn insert
	"Takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new list with the same elements as lst but inserting n in its corresponding place"
	[x l]
	(cond
		(empty? l) (cons x l)
		(> (first l) x) (cons x l) 
		:else (cons (first l) (insert x (rest l)))))

; 8
(defn my-sort
	"Takes an unordered list of numbers as an argument, and returns a new list with the same elements but in ascending order."
	[l]
	(loop 
		[l l
		r ()]
		(if (empty? l)
		r
		(recur (rest l) (insert (first l) r)))))


; 9
(defn binary
	"Returns a list with a sequence of ones and zeros equivalent to the binary representation of n"
	[x]
	(loop
		[x x
		r ()]
		(if (zero? x)
			r
			(recur (quot x 2) (cons (rem x 2) r)))))

; 10
(defn prime-factors
	"Returns a list with a sequence of ones and zeros equivalent to the binary representation of n"
	[x]
	(loop
		[x x
		d 2
		r ()]
		(cond
			(= x 1) (reverse r)
			(zero? (rem x d)) (recur (quot x d) d (cons d r))
			:else (recur x (inc d) r))))


; 11
(defn compress
	"Returns a list containing the prime factors of n in ascending order."
	[l]
	(loop
		[l l
		a nil
		r ()]
		(cond 
			(empty? l) (reverse r)
			(not= a (first l)) (recur (rest l) (first l) (cons (first l) r))
			:else (recur (rest l) (first l) r))))

; 12
(defn pack
	"Takes a list lst as its argument. If lst contains consecutive repeated elements, they should be replaced with a single copy of the element. "
	[li]
	(loop
		[l (rest li)
		a (first li)
		r ()
		li (cons a ())]
		(cond 
			(nil? a) (reverse r)
			(empty? l) (reverse (cons li r))
			(not= a (first l)) (recur (rest l) (first l) (cons li r) (cons (first l) ()))
			:else (recur (rest l) (first l) r (cons a li)))))

; 13
(defn encode
	"Takes a list lst as its argument. If lst contains consecutive repeated elements they should be placed in separate sublists"
	[li]
	(loop
		[l (rest li)
		a (first li)
		r ()
		n 1]
		(cond
			(nil? a) (reverse r)
			(empty? l) (reverse (cons [n a] r))
			(not= a (first l)) (recur (rest l) (first l) (cons [n a] r) 1)
			:else (recur (rest l) (first l) r (inc n)))))

; 14
(defn encode-modified
	"Consecutive duplicates of elements in lst are encoded as vectors [n e], where n is the number of duplicates of the element e"
	[li]
	(loop
		[l (rest li)
		a (first li)
		r ()
		n 1]
		(cond
			(nil? a) (reverse r)
			(empty? l) (if (= n 1)
				(reverse (cons a r))
				(reverse (cons [n a] r)))
			(not= a (first l)) (if (= n 1)
				(recur (rest l) (first l) (cons a r) 1)
				(recur (rest l) (first l) (cons [n a] r) 1))
			:else (recur (rest l) (first l) r (inc n)))))

; 15
(defn decode
	"It works the same as the previous problem, but if an element has no duplicates it is simply copied into the result list"
	[li]
	(loop 
		[l li
		n 0
		a nil
		r ()]
		(cond
			(and (zero? n) (empty? l)) (reverse r)
			(> n 0) (recur l (dec n) a (cons a r))
			(vector? (first l)) (recur (rest l) (first (first l)) (last (first l)) r)
			:else (recur (rest l) 0 (first l) (cons (first l) r)))))


; test 1
(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

; test 2
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]) (invert-pairs '([a 1][a 2][b 1][b 2]))))
  (is (= '([1 January][2 February][3 March]) (invert-pairs '([January 1][February 2][March 3])))))

; test 3
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

; test 4
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

; test 5
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four) 
         (my-flatten '(((one) ((two))) () (three (())) four)))))

; test 6
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))

; test 7
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

; test 8
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

; test 9
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

; test 10
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

; test 11
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

; test 12
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

; test 13
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

; test 14
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

; test 15
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))


(run-tests)

