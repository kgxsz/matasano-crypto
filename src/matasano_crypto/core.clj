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
  (->> (slurp "resources/challenge-four-input.txt")
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
  (let [bs (readers/read-base64-string (clojure.string/replace (slurp "resources/challenge-six-input.txt") #"\n" ""))
        key-sizes (range 2 41)
        normalised-hamming-distances (for [key-size key-sizes]
                                       (let [blocks (map byte-array (partition key-size bs))
                                             average #(/ % (* 2 (count blocks)))
                                             normalise #(float (/ % key-size))]
                                         {:key-size key-size
                                          :normalised-hamming-distance (->> (interleave blocks blocks)
                                                                            (rest)
                                                                            (partition 2)
                                                                            (map (partial apply utils/hamming-distance))
                                                                            (reduce +)
                                                                            (average)
                                                                            (normalise))}))
        most-likely-key-size (->> normalised-hamming-distances
                                  (sort-by :normalised-hamming-distance)
                                  (first)
                                  (:key-size))
        transposed-blocks (for [i (range most-likely-key-size)]
                            (->> (partition-all most-likely-key-size bs)
                                 (keep #(nth % i nil))
                                 (byte-array)))
        key (->> (map utils/crack-length-one-key-repeating-XOR-encryption transposed-blocks)
                 (map (comp first vec :key))
                 (byte-array))]
    (writers/write-ASCII-string (utils/apply-repeating-XOR-cipher key bs))))


(defn challenge-seven
  []
  (let [bs (readers/read-base64-string (clojure.string/replace (slurp "resources/challenge-seven-input.txt") #"\n" ""))
        k (readers/read-ASCII-string "YELLOW SUBMARINE")]
    (writers/write-ASCII-string (utils/decrypt-AES-in-ECB-mode k bs))))
