;----------------------------------------------------------
; Activity: Using the Sequence API
; Date: February 25, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------

(use 'clojure.test)


(defn positives
	"Takes a list of numbers lst as its argument, and returns a new list that only contains the positive numbers of lst"
	[l]
	(filter #(< 0 %) l))


(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))


(defn dot-product
	"Takes two arguments: the lists a and b. It returns the result of performing the dot product of a times b."
	[la lb]
	(reduce + (map #(* %1 %2) la lb)))

(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))


(defn pow
	"Takes two arguments as input: a number a and a positive integer b. It returns the result of computing a raised to the power b"
	[a b]
	(nth (iterate #(* % a) 1) b))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))


(defn replic
	"Takes two arguments: a list lst and an integer number n, where n ≥ 0. It returns a new list that replicates n times each element contained in lst"
	[n l]
	(flatten (map #(repeat n %) l) ))


(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) (replic 4 '(1 2 3 4)))))


(defn expand
	"Takes a list lst as its argument. It returns a list where the first element of lst appears one time, the second elements appears two times, the third element appears three times, and so on."
	[l]
	(flatten (map-indexed (fn [i x] (repeat (inc i) x )) l)))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e) (expand '(a b c d e)))))


(defn largest
	"Takes as argument a nonempty list of numbers lst. It returns the largest value contained in lst."
	[l]
	(reduce #(if (< %1 %2) %2 %1) l))


(deftest test-largest
  (is (= 31 (largest '(31))))
  (is (= 5 (largest '(1 2 3 4 5))))
  (is (= -1 (largest '(-1 -2 -3 -4 -5))))
  (is (= 52 (largest '(32 -1 45 12 -42 52 17 0 21 2)))))


(defn drop-every
	"Takes two arguments: an integer number n, where n ≥ 1, and a list lst. It returns a new list that drops every n-th element from lst"
	[n l]
	(map
		(fn [[x y]] y)
	(filter 
		(fn [[x y]] (not= (rem x n) 0)) 
		(map-indexed (fn [i x] [(inc i) x]) l))))

(deftest test-drop-every
  (is (= () (drop-every 5 ())))
  (is (= '(1 2 3) (drop-every 4 '(1 2 3 4))))
  (is (= '(1 3 5 7) (drop-every 2 '(1 2 3 4 5 6 7 8))))
  (is (= '(1 3 5 7 9) (drop-every 2 '(1 2 3 4 5 6 7 8 9))))
  (is (= '(a b d e g h j) (drop-every 3 '(a b c d e f g h i j))))
  (is (= '(a b c d e f g h i j) 
         (drop-every 20 '(a b c d e f g h i j))))
  (is (= () (drop-every 1 '(a b c d e f g h i j)))))


(defn gcd
	"Takes two positive integer arguments a and b as arguments, where a > 0 and b > 0. It returns the greatest common divisor (GCD) of a and b."
	[a b]
	(second 
		(last 
			(take-while 
				(fn [[x y]] (not= y 0)) 
				(iterate (fn [[x y]] [y (rem x y)]) [a b]) ))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(defn rotate
	[n l]
	(map (fn [[i x]] x)
		(filter (fn [[i x]] (<=  n  i (+ (dec n) (count l))))
		(map-indexed 
			(fn [i x] [i x])
	 		(flatten (repeat 2 l))))))

(defn rotate-left
	"Takes two arguments: an integer number n and a list lst. It returns the list that results from rotating lst a total of n elements to the left. If n is negative, it rotates to the right"
	[n l]
	(cond 
		(empty? l) ()
		(neg? n) (rotate (+ (count l) (rem n (count l))) l)
		:else (rotate (rem n (count l)) l)))



(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))  
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))


(defn insert
  "Returns a new list with the same elements as l 
  but inserting x in the i index"
  [i x l]
    (concat (first (split-at i l))
            (list x) 
            (second (split-at i l))))

(defn insert-everywhere
	"takes two arguments as input: an object x and a list lst. It returns a new list with all the possible ways in which x can be inserted into every position of lst"
	[x l]
	(map 
		(fn [[i l]] (insert i x l) )
	(map-indexed 
		(fn [i x] [i x])
	(repeat (inc (count l)) l)))
	)

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
          (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
          (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))


(run-tests)