;----------------------------------------------------------
; Activity: Recursive Functions, Part I
; Date: January 28, 2016.
; Authors:
;          A01165749 Brian Flores Gonzalez
;----------------------------------------------------------


(use 'clojure.test)



(defn my-count
	"Returns the number of elements contained in its input list."
	[l]
	(if (empty? l)
		0
		(+ 1 (my-count (rest l)))))



(defn add-list
	"Returns the sum of all the elements of its input list, or 0 if its empty."
	[l]
	(if (empty? l)
		0
		(+ (first l) (add-list (rest l)))))

(defn member?
	"Takes two arguments, any data x and a list lst. Returns true if x is contained in lst, false otherwise."
	[x l]
	(if (empty? l)
		false
		(or (= x (first l)) (member? x (rest l)))))


(defn list-of-symbols?
	"Takes a list lst as its argument. It returns true if all the elements (possibly zero) contained in lst are symbols, or false otherwise."
	[l]
	(if (empty? l)
		true
		(and (symbol? (first l)) (list-of-symbols? (rest l)))))

(defn my-last
	"Returns the last element of its input list, or nil of its empty."
	[l]
	(if (empty? l)
		nil
		(if (empty? (rest l))
			(first l)
			(my-last (rest l)))))

(defn cons-end
	"Takes two arguments, any data x and a list lst. Returns a list composed by the same elements of lst but with x at the end."
	[x l]
	(if (empty? l)
		(cons x l)
		(cons (first l) (cons-end x (rest l) ) )))

(defn my-reverse
	"Takes a list as an argument. It returns another list with the same elements as the input list, but in reverse order."
	[l]
	(loop [l l
		r ()]
		(if (empty? l)
			r
			(recur (rest l) (cons (first l) r)))))

(defn my-butlast
	"Returns a list with the same elements as its input list but excluding the last element, or nil of its empty."
	[l]
	(if (empty? l)
		nil
		(if (empty? (rest l))
			()
			(cons (first l) (my-butlast (rest l))))))

(defn my-concat
	"Returns the resulting list of appending the two lists it takes as input."
	[la lb]
	(if (empty? la)
		lb
		(cons (first la) (my-concat (rest la) lb))))

(defn deep-reverse
	"takes a list as its input. It returns a list with the same elements as its input but in reverse order. If there are any nested lists, these too should be reversed"
	[l]
	(cond 
		(empty? l) ()
		(list?  (last l)) (cons (deep-reverse (last l)) (deep-reverse (butlast l)))
		:else (cons (last l) (deep-reverse (butlast l)))))



(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(n m (l k) j ((i h g) f e d) c b a) (deep-reverse '(a b c ( d e f ( g h i)) j (k l) m n))))	
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))
  


(run-tests)