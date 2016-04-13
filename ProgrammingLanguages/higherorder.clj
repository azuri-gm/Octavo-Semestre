;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01165749 Brian Flores González
;----------------------------------------------------------

(use 'clojure.test)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true 
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(defn my-map-indexed
  "Takes two arguments: a function f and a list lst. It returns a list consisting of the result of applying f to 0 and the first item of lst, followed by applying f to 1 and the second item in lst, and so on until lst is exhausted"
	[f l]
	(loop [f f
		l l
		i 0
		r ()]
		(cond
			(empty? l) (reverse r)
			:else (recur f (rest l) (inc i) (cons (f i (first l)) r))
			)))

(defn my-drop-while
  "takes two arguments: a function f and a list lst. It returns a list of items from lst dropping the initial items that evaluate to true when passed to f. Once a false value is encountered, the rest of the list is returned"
	[p l]
	(cond
		(empty? l) ()
		(not (p (first l))) l
		:else (my-drop-while p (rest l))))

(defn abs
  "Alternative to the Math/abs function"
  [n]
  (max n (- n)))

(defn bisection
  "Takes a, b, and f as arguments. It finds the corresponding root using the bisection method. The algorithm must stop when a value of c is found such that: abs(f(c)) < 1.0×10-15."
  [a b f]
  (loop [a a 
        b b 
        c (/ (+ a b) 2)]
    (cond 
      (< (abs (f c)) 1.0E-15)  c
      (< (* (f a) (f c)) 0)  (recur a c (/ (+ a c) 2))
      :else  (recur c b (/ (+ b c) 2)))))

(defn deriv
  "Takes f and h as its arguments, and returns a new function that takes x as argument, and which represents the derivate of f given a certain value for h."
  [f h]
  (fn [x]
    (/ (- (f (+ h x)) (f x)) h)))

(defn integral
  "Takes as arguments a, b, n, and f. It returns the value of the integral, using Simpson's rule."
  [a b n f]
  (loop [h (/ (- b a) n)
        z (/ h 3)
        f f
        n n
        k 0
        s 0]
        (cond 
        (> k n) s
        (= k 0) (recur h z f n (inc k) (+ (* z (f (+ a (* k h)))) s) )
        (= k n) (recur h z f n (inc k) (+ (* z (f (+ a (* k h)))) s) )
        (even? k) (recur h z f n (inc k) (+ (* (* 2 z) (f (+ a (* k h)))) s) )
        :else (recur h z f n (inc k) (+ (* (* 4 z) (f (+ a (* k h)))) s)))))




(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))



(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))


(deftest test-bisection
  (is (aprox= 0.0001 3.0 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 -4.0 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 1.618033988749895
                     (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001 -0.6180339887498948
                     (bisection -10 1 (fn [x] (- (* x x) x 1))))))


(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))


(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y)))))))) 



(run-tests)