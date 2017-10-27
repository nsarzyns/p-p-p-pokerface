(ns p-p-p-pokerface)

(defn rank [card]
 (let [[Rank Suit] card]

   (if (Character/isDigit Rank)
     (Integer/valueOf (str Rank))
     ({\T 10, \J 11, \Q 12, \K 13, \A 14} Rank)
     )))


(defn suit [card]
  (let [[Rank Suit] card]
    (str Suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
 (or (= 2 (get (frequencies (vals (frequencies (map rank hand))))2))
  (= 1 (get (frequencies (vals (frequencies (map rank hand))))4))))


(defn straight? [hand]
(let [sorted-hand-by-rank (sort (map rank hand))
        sorted-hand-by-rank-ace (sort (replace {14 1} (map rank hand)))
        sorted-hand-by-rank-lowest-card (first sorted-hand-by-rank)]
  (or (= sorted-hand-by-rank (range sorted-hand-by-rank-lowest-card (+ sorted-hand-by-rank-lowest-card 5)))
      (= sorted-hand-by-rank-ace (range 1 6)))))

(defn straight-flush? [hand]
 (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ;

(defn value [hand]
(cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))




