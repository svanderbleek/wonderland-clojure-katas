(ns alphabet-cipher.coder)

(def a-z
  (map char (range 97 123)))

(def a-z*
  (cycle a-z))

(def a-z-prod
  (for [c1 a-z c2 a-z] [c1 c2]))

(def a-z-offset
  (->> (range 0 26)
       (map #(take 26 (drop % a-z*)))
       flatten))

(def cipher
  (zipmap a-z-prod a-z-offset))

(defn make-rev [rev [secret, message] encoded]
  (assoc rev [secret, encoded] message))

(def rev-cipher
  (reduce-kv make-rev {} cipher))

(defn make-de [de [secret, message] encoded]
  (assoc de [message, encoded] secret))

(def de-cipher
  (reduce-kv make-de {} cipher))

(defn make-secret [keyword message]
  (take (count message) (cycle keyword)))

(defn lookup [str1 str2 table]
    (->> (interleave str1 str2)
         (partition 2)
         (map #(get table %))
         (apply str)))

(defn encode [keyword message]
  (lookup (make-secret keyword message) message cipher))

(defn decode [keyword message]
  (lookup (make-secret keyword message) message rev-cipher))

(defn try-keys [cycled message encoded]
  (loop [n 1]
    (let [key (apply str (take n cycled))]
      (if (or (= (encode key message) encoded) (= n (count cycled)))
        key
        (recur (inc n))))))

(defn decipher [encoded message]
  (try-keys (lookup message encoded de-cipher) message encoded))
