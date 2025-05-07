(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (boolean
   (and (seq coll)
        (empty? (rest coll)))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (recur (rest coll))))

(defn max-element [a-seq]
  (when (seq a-seq)
    (reduce max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (recur
           (seq-max (first a-seq) (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (lazy-seq
   (when-let [[head & tail] (seq a-seq)]
     (if (pred? head)
       (cons head (my-filter pred? tail))
       (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (lazy-seq
   (when-let [[head & tail] (seq a-seq)]
     (cond (pred? head)
           (cons head (my-take-while pred? tail))))))

(defn my-drop-while [pred? a-seq]
  (lazy-seq
   (when-let [[head & tail] (seq a-seq)]
     (if (pred? head)
       (my-drop-while pred? tail)
       (cons head tail)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons
           (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    `()
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse
   (map reverse
        (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (map concat
         (drop-last (tails a-seq))
         (drop-last (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [head (first a-seq)
          cnt (get freqs head 0)
          freqs' (assoc freqs head (inc cnt))]
      (recur freqs' (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)
          remaining (rest a-map)]
      (concat (repeat v k)
              (un-frequencies remaining)))))

(defn eager-my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) 
          (eager-my-take (dec n) (rest coll)))))

(defn my-take [n coll]
  (lazy-seq
   (when-let [[x & xs] (seq coll)]
     (when (pos? n)
       (cons x (my-take (dec n) xs))))))

(defn my-drop [n coll]
  (lazy-seq
   (cond
     (<= n 0) (seq coll)
     (seq coll) (let [[_ & xs] (seq coll)]
                  (my-drop (dec n) xs))
     :else nil)))

(defn halve [a-seq]
  (let [n (quot (count a-seq) 2)
        first-half (my-take n a-seq)
        second-half (my-drop n a-seq)]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [[x & xs] a-seq 
                [y & ys] b-seq] 
            (if (<= x y)
              (cons x (seq-merge xs b-seq)) 
              (cons y (seq-merge a-seq ys))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) 
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) 
                 (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

