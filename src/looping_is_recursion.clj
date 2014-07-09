(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp accum]
                 (if (zero? exp)
                   accum
                   (recur base
                          (dec exp)
                          (* accum
                             base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq prev-element]
                 (if (empty? a-seq)
                   prev-element
                   (recur (rest a-seq)
                          (first a-seq))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) (empty? seq2)
   (empty? seq2) false
   (not (= (first seq1)
           (first seq2))) false
   :else (recur (rest seq1)
                (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         index 0]
    (cond (empty? a-seq) nil
          (pred (first a-seq)) index
          :else (recur (rest a-seq)
                       (inc index)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         sum 0
         count 0]
    (if (empty? a-seq)
      (/ sum count)
      (recur (rest a-seq)
             (+ sum (first a-seq))
             (inc count)))))

(defn parity [a-seq]
  (loop [a-seq a-seq
         odd-elements #{}]
    (if (empty? a-seq)
      odd-elements
      (let [cur (first a-seq)]
        (recur (rest a-seq)
               (if (contains? odd-elements cur)
                 (disj odd-elements cur)
                 (conj odd-elements cur)))))))

(defn fast-fibo [n]
  (loop [cur 0
         fn-2 -1
         fn-1 1]
    (if (= cur n)
      (+ fn-2
         fn-1)
      (recur (inc cur)
             fn-1
             (+ fn-2
                fn-1)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         elements-seen #{}
         unique-elements []]
    (let [cur (first a-seq)]
      (if (or (empty? a-seq)
              (contains? elements-seen cur))
        unique-elements
        (recur (rest a-seq)
               (conj elements-seen cur)
               (conj unique-elements cur))))))
