;;; ITESM CEM, April 14, 2016.
;;; Clojure Source File
;;; Activity: Macros
;;; Author:
;;;          A01165988 Eduardo Azuri Gaytán Martínez

(ns macros)

(defmacro my-or
  ([] true)
  ([x] x)
  ([x & y]
   `(let [temp# ~x]
      (if temp#
          temp#
        (my-or ~@y)))))

(my-or)
(my-or false :one nil :two false :three)
(my-or false false nil)
(my-or nil nil false)

(defmacro do-loop
  ([& lst]
   `(let [w# (first (last (quote ~lst)))
          r# (first (rest (last (quote ~lst))))]
      (loop []
        ~@lst
        (cond
         (= :until w#) (when-not (eval r#) (recur))
         (= :while w#) (when (eval r#) (recur)))))))

(defmacro def-pred
  ([nam arg & body]
   `(do (defn ~nam ~arg (do ~@body))
      (defn ~(symbol (str "not-" nam)) ~arg (not (do ~@body))))))

(defmacro defn-curry
  ([nam args & body]
   (if (empty? args)
   `(defn ~nam ~args ~@body)
     (let [funs (reduce (fn [f s]
                           `(letfn [(help#
                                     ([] help#)
                                     ([x#] (let [~s x#] ~f))
                                     ([x# & r#] (let [~s x#] (apply (help# x#) r#))))]
                              help#))
                         `(do ~@body) (reverse args))]
        `(defn ~nam [& args#]
           (let [help# ~funs]
             (apply help# args#)))))))
