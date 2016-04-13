(defrecord Book [author title year publisher])

(defn books-in-range [b x y]
	(filter #(<= x (:year %) y) b)
	)

;(println (books-in-range [(->Book "A" "L" 1943 "G")
;	(->Book "C" "T" 1955 "T")
;	(->Book "G" "A" 1945 "S")
;	] 1940 1945))


(defn gcd [x y]
	(->> (iterate (fn [[a b]] (cond 
		(= a b) [a a]
		(> a b) [(- a b) b]
		:else [a (- b a)] ))

	 [x y])
		(take-while (fn [[x y]] (not= x y)))
		last

		first
		))

(println (gcd 6307 1995))