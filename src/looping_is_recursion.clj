(ns looping-is-recursion)

(defn power [base exp]
  ((fn [acc exp]
     (if (zero? exp)
       acc
       (recur (* acc base) (dec exp))))
   1 exp))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [f (first a-seq)
          r (rest  a-seq)]
      (if (empty? r)
        f
        (recur r)))))

(defn seq= [seq1 seq2]
  (if (and (empty? seq1) (empty? seq2))
    true
    (if (not= (first seq1) (first seq2))
      false
      (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [pos 0
         lseq a-seq]
    (if (empty? lseq)
      nil
      (if (pred (first lseq))
        pos
        (recur (inc pos) (rest lseq))))))

(defn avg [a-seq]
  (loop [sum 0
         c   0
         lseq a-seq]
    (if (empty? lseq)
      (if (== 0 c)
        0
        (/ sum c))
      (recur (+ sum (first lseq)) (inc c) (rest lseq)))))

(defn parity [a-seq]
  (let [toggle (fn [seq1 elem]
        (if (contains? seq1 elem)
          (disj seq1 elem)
          (conj seq1 elem)))]
    (loop [res #{}
           seq2 a-seq]
      (if (empty? seq2)
        res
        (recur (toggle res (first seq2)) (rest seq2))))))

(defn fast-fibo [n]
  (let [x0 0
        x1 1]
    (cond
      (< n 0) nil
      (== n 0) x0
      (== n 1) x1
      :else (loop [cn 2
                   m2 x0
                   m1 x1]
              (let [curr (+ m2 m1)]
                (if (== cn n)
                  curr
                  (recur (inc cn) m1 curr)))))))

(defn cut-at-repetition [a-seq]
  (vec (loop [res #{}
              seq1 a-seq]
         (if (empty? seq1)
           res
           (let [f (first seq1)
                 r (rest  seq1)]
             (if (contains? res f)
               res
               (recur (conj res f) r)))))))


;----

(defn recursive-factorial [n]
  (if (zero? n)
    1
    (* n (recursive-factorial (dec n)))))

(defn helper [acc n]
  (if (zero? n)
    acc
    (helper (* acc n) (dec n))))

(defn accumulating-factorial [n]
  (helper 1 n))

(defn recur-factorial [number]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc n) (dec n))))]
    (helper 1 number)))

(defn loopy-factorial [down-from]
  (loop [acc 1
         n down-from]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))

