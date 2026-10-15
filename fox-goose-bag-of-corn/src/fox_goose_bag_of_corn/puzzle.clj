(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def end-pos   [[[] [:boat] [:fox :goose :corn :you]]])

(defn you? [el]
  (= :you el))

(defn only-you? [boat]
  (= (set boat) #{:boat :you}))

(defn remove-you [place]
  (vec (remove you? place)))

(defn boat? [el]
  (= :boat el))

(defn remove-boat [place]
  (vec (remove boat? place)))

(defn you-and? [place]
  (and (some you? place) (> (count (remove-boat place)) 1)))

(defn you-moves [[left boat right]]
  (cond
    (some you? left)  [[(remove-you left) (conj boat :you)  right]]
    (only-you? boat)  [[(conj left :you)  (remove-you boat) right]
                       [left              (remove-you boat) (conj right :you)]]
    (some you? right) [[left              (conj boat :you)  (remove-you right)]]))

(defn you-pairs [place]
  (map #(vector :you %) (remove-you place)))

(defn remove-pair [place pair]
  (vec (remove (set pair) place)))

(defn obj-moves [[left boat right]]
  (cond
    (you-and? left)  (mapv (fn [pair] [(remove-pair left pair)        (into boat pair) right])                    (you-pairs left))
    (you-and? boat)  [                [(into left (remove-boat boat)) [:boat]          right]
                                      [left                           [:boat]          (into right (remove-boat boat))]]
    (you-and? right) (mapv (fn [pair] [left                           (into boat pair) (remove-pair right pair)]) (you-pairs right))
    :else []))

(defn all-moves [state]
  (shuffle (into [] (concat (you-moves state) (obj-moves state)))))

(defn eating?
  ([left _ right]
    (or (eating? left) (eating? right)))
  ([bank]
    (if (some #(= :you %) bank)
      false
      (let [bank-set (set bank)]
        (or (every? bank-set #{:fox :goose})
            (every? bank-set #{:goose :corn}))))))

(defn losing? [state]
  (if (and state (not (apply eating? state)))
    false
    true))

(defn done? [[_ _ right]]
  (= (set right) #{:fox :goose :corn :you}))

(defn river-crossing-plan []
  (loop [path  []
         state (first start-pos)
         moves (all-moves state)]
    (if (done? state)
      (conj path state)
      (let [next (first moves)]
        (if (losing? next)
          (recur path state (rest moves))
          (recur (conj path state) next (all-moves next)))))))
