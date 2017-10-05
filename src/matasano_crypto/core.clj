(ns matasano-crypto.core
  (:require [matasano-crypto.readers :as readers]
            [matasano-crypto.writers :as writers]
            [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (writers/write-base64-string (readers/read-hex-string s)))


(defn challenge-two
  [s1 s2]
  (writers/write-hex-string (utils/XOR (readers/read-hex-string s1)
                                             (readers/read-hex-string s2))))


(defn challenge-three
  [s]
  (let [bs (readers/read-hex-string s)
        {:keys [value]} (utils/crack-length-one-key-repeating-XOR-encryption bs)]
    (writers/write-ASCII-string value)))


(defn challenge-four
  []
  (->> (slurp "http://cryptopals.com/static/challenge-data/4.txt")
       (clojure.string/split-lines)
       (map readers/read-hex-string)
       (map utils/crack-length-one-key-repeating-XOR-encryption)
       (sort-by :score >)
       (first)
       (:value)
       (writers/write-ASCII-string)))


(defn challenge-five
  [k s]
  (writers/write-hex-string
   (utils/apply-repeating-XOR-cipher (readers/read-ASCII-string k)
                                     (readers/read-ASCII-string s))))


(defn challenge-six
  []
  (let [url "http://cryptopals.com/static/challenge-data/6.txt"
        bs (readers/read-base64-string (clojure.string/replace (slurp url) #"\n" ""))
        key-sizes (range 2 41)
        block-analysis (for [key-size key-sizes]
                         (let [blocks (take (* 4 key-size) bs)
                               block-1 (take key-size blocks)
                               block-2 (drop key-size (take (* 2 key-size) blocks))
                               block-3 (drop (* 2 key-size) (take (* 3 key-size) blocks))
                               block-4 (drop (* 3 key-size) blocks)
                               hamming-distance (-> (utils/hamming-distance (byte-array block-1)
                                                                            (byte-array block-2))
                                                    (+ (utils/hamming-distance (byte-array block-3)
                                                                               (byte-array block-4)))
                                                    (/ (* 2 key-size))
                                                    (float))]

                           (println {:key-size key-size :hamming-distance hamming-distance})
                           {:key-size key-size :hamming-distance hamming-distance}))
        most-likey-key-size (:key-size (first (sort-by :hamming-distance block-analysis)))]

    (println most-likey-key-size)
    (for [i (range most-likey-key-size)]
      (->> (partition-all most-likey-key-size bs)
           (keep #(nth % i nil))
           (byte-array)
           (utils/crack-length-one-key-repeating-XOR-encryption)
           (:key)
           (vec)))
    ))

(challenge-six)

