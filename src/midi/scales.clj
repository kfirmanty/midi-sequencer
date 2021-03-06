(ns midi.scales)

(def scales
  {:pentatonic-minor [[0 10] [3 20] [5 20] [7 20] [10 20] [12 10]]
   :phrygian-dominant [[0 10] [1 10] [4 20] [5 20] [7 20] [8 10] [10 10] [12 10]]
   :harmonic-fourth [[0 10] [2 10] [3 20] [6 10] [7 20] [8 10] [10 10] [12 10]]
   :japan [[0 20] [1 20] [5 20] [7 20] [8 20]]})

(def ^:const base 69)

(defn random-pitch-val []
  (int (+ base (* 24 (- 0.5 (Math/random))))))

(defn random-from-scale [scale-key]
  (let [scale (scale-key scales)
        step-num (int (* (Math/random) (count scale)))]
    (+ base (-> scale (nth step-num) first))))

(defn get-note [scale weight]
  (loop [step 0 sum-weight 0]
    (let [[note note-weight] (nth scale step)]
      (if (< weight (+ sum-weight note-weight))
        note
        (recur (inc step) (+ sum-weight note-weight))))))

(defn random-weighted [scale-key]
  (let [scale (scale-key scales)
        scale-sum (->> scale (map #(second %)) (reduce + 0))
        weight (-> (Math/random) (* scale-sum) int)
        note (get-note scale weight)]
    (+ note base)))

(defn get-note-step-num [note scale]
  (let [note-base (Math/abs (- note base))]
    (loop [step 0]
      (if (= note-base (first (nth scale step)))
        step
        (recur (inc step))))))

(defn even-out-note [note prev scale]
  (let [note-num (get-note-step-num note scale)
        prev-num (get-note-step-num prev scale)
        max-diff 2
        diff (Math/abs (- note-num prev-num))
        diff-fn (if (< note-num prev-num) - +)
        new-step (if (> diff max-diff)
                   (diff-fn prev-num max-diff)
                   note-num)]
    (+ base (first (nth scale new-step)))))

(defn even-out [[a & r] scale-key]
  (let [scale (scale-key scales)]
    (loop [step-num 0 new-steps [a]]
      (if (< step-num (count r))
        (recur (inc step-num)
               (conj new-steps
                     (even-out-note (nth r step-num) (nth new-steps step-num) scale)))
        new-steps))))

(defn get-steps [len] (into [] (for [i (range (inc len))]
                                 {:type :note-on
                                  :pitch (random-weighted :phrygian-dominant)
                                  :velocity 100
                                  :length 4})))

(defn gcd [a b acc]
  (if (zero? b)
    acc
    (recur b (mod a b) (conj acc [a b]))))

(defn fpass-euc [a b]
  [(flatten [1 (repeat (- (/ a b) 1) 0)]) (repeat (mod a b) 0)])

(defn e [a b]
  (let [[fs lls] (fpass-euc a b)
        ls (vector (first lls))
        seqs (gcd b (mod a b) [])]
    (if (empty? lls)
      (repeat (/ a (count fs)) fs)
      (loop [f fs l ls seqs seqs]
        (let [[sf sl] (first seqs)]
          (if (or (= sl 0) (= sl 1))
            (concat (repeat sf f) (repeat sl l))
            (recur (concat f l) f (rest seqs))))))))
