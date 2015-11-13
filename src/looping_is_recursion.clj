(ns looping-is-recursion)

(defn power [base exp]
 (let [helper
  (fn [acc b e]
    (if (zero? e)
      acc
      (recur (* acc b) b (dec e))))]
  (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (empty? seq1) (empty? seq2)
    (empty? seq2) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         curr a-seq]
   (cond
    (empty? curr) nil
    (pred (first curr)) n
    :else (recur (inc n) (rest curr)))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         curr a-seq]
    (if (empty? curr)
      (/ sum n)
      (recur (inc n) (+ sum (first curr)) (rest curr)))))

(defn parity [a-seq]
  (loop [curr-set #{}
         curr a-seq]
   (if (empty? curr)
    curr-set
    (recur (if (contains? curr-set (first curr))
      (disj curr-set (first curr))
      (conj curr-set (first curr))) (rest curr)))))

(defn fast-fibo [n]
  (loop [fib_n-1 0
         fib_n 1
         n n]
    (cond
      (= 0 n) 0
      (= 1 n) fib_n
      :else (recur fib_n (+ fib_n-1 fib_n) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         curr a-seq]
   (if (or (empty? curr)
           (= (first curr) (first result)))
    result
    (recur (conj result (first curr)) (rest curr)))))

