(use 'clojure.test)

(defn sum-quare-difference
	[n]
	(let [a  (reduce + (take n (iterate inc 1)))
		b (reduce + (map #(* %1 %1) (take n (iterate inc 1))))]
		(- (* a a) b)))

;(println (sum-quare-difference 100))

(defn is-prime? 
	[x]
	(cond 
		(= x 2) true
		(even? x) false 
		:else (let [lim (num (int (Math/sqrt x)))]
		(loop [i 3]
			(cond 
				(> i lim) true
				(zero? (mod x i)) false
				:else (recur (+ i 2)))))))

(defn nth-prime
	[n]
	(last (take n (filter is-prime? (iterate inc 2)))))

(println (nth-prime 10001))

(defn sum-primes
	[n]
	(reduce + (take-while #(> n %1) (filter is-prime? (iterate inc 2)))))

;(print (sum-primes 2000000))

(defn divisible-range
	[x a b]
	(every? #(zero? (rem x %1)) (take b (iterate inc a))))


(defn smallest-multiple
	[a b]
	(first (drop-while #(not (divisible-range %1 a b)) (iterate inc 1))))

;(println (smallest-multiple 1 20))


(defn reverse-num
	 [n]
  (Integer. (clojure.string/reverse (str n))))

(defn palindrome?
	[n]
	(= n (reverse-num n)))

(defn largest-palindrome-product
	[a b]
	(->>
		(for [x (range a b) y (range a b)] (* x y))
		(filter palindrome?)
		(apply max)))

;(println (largest-palindrome-product 100 999))


(defn aprox=
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))


(defn special-pythagorean-triplet
	[n]
	(->>
	(for [x (range 1 n) y (range 1 n)] (list x y (Math/sqrt (+ (* x x) (* y y)))))
	(filter (fn 
		[l] 
		(let [a (first l) b (second l) c (last l)]
			(aprox= 0.0001 (+ a b c) n))))
	first
	(#(* (first %1) (second %1) (last %1)))))
;(println (format "%.0f" (special-pythagorean-triplet 1000)))


(defn power-digit-sum
	[n p]
	(->>
		(last (take p (iterate #(*' % n) n)))
		str
		seq
		(map #(- (int %1) 48))
		(reduce +)))

;(println (power-digit-sum 2 1000))

(defn fib-seq
	[d]
	(->>
		(iterate (fn [[a b]] [b (+' b a)]) [0 1])
		(map-indexed (fn [i [a b]] [(inc i) b]))
		(drop-while (fn [[a b]] (< (count (str b)) d)))
		first
		first
		))
;(println (fib-seq 1000))

(defn  largest-prime-factor
	[n]
	(->>
		(filter is-prime? (iterate inc 2))
		(take-while #(< % (Math/sqrt n)))
		(filter #(zero? (mod n %)))
		(apply max)))

;(println (largest-prime-factor 600851475143))

(defn fact 
	[n]
	(if (zero? n) 1N
		(*' n (fact (dec n)))))


(defn factorial-digit-sum
	[n]
	(->>
		(fact n)
		str
		seq
		(map #(- (int %1) 48))
		(reduce +)))

;(println (factorial-digit-sum 100))


(defn count-collatz
	[n]
	(->>
		(iterate #(if (even? %) (/ % 2) (inc (* % 3))) n)
		(take-while #(> % 1))
		count
		inc))


(defn longest-collatz-sequence
	[n]
	(->>
		(range 1 n)
		(map #(vector %1 (count-collatz %1)))
		(reduce (fn [[a b] [c d]] (if (> b d) [a b] [c d])))
		first
		))

;(println (longest-collatz-sequence 1000000))

(defn highly-divisible-triangular-number
	[n]
	(->>
		(iterate inc 1)
		(map #(* %1 (/ (+ %1 1) 2)))
		(map (fn [x] [x 
			(inc (* 2 (count (filter #(zero? (mod x %1)) (range 2 (inc (int (Math/sqrt x))))))))]))
		(drop-while (fn [[x d]] (< d n)))
		first
		first))

;(println (highly-divisible-triangular-number 500))