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
  (->> (slurp "resources/challenge-four.txt")
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
  (let [bs (readers/read-base64-string (clojure.string/replace (slurp "resources/challenge-six.txt") #"\n" ""))
        key-sizes (range 2 41)
        hamming-distances (for [key-size key-sizes]
                            (let [blocks (map byte-array (partition key-size bs))
                                  average #(/ % (* 2 (count blocks)))
                                  normalise #(float (/ % key-size))]
                              {:key-size key-size
                               :hamming-distance (->> (interleave blocks blocks)
                                                      (rest)
                                                      (partition 2)
                                                      (map (partial apply utils/hamming-distance))
                                                      (reduce +)
                                                      (average)
                                                      (normalise))}))
        most-likely-key-size (:key-size (first (sort-by :hamming-distance hamming-distances)))]

    most-likely-key-size

    ))
