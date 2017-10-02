(ns matasano-crypto.core
  (:require [matasano-crypto.readers :as readers]
            [matasano-crypto.writers :as writers]
            [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (writers/write-base64-string (readers/read-hex-string s)))


(defn challenge-two
  [s1 s2]
  (writers/write-hex-string (utils/fixed-XOR (readers/read-hex-string s1) (readers/read-hex-string s2))))


(defn challenge-three
  [s]
  (:plaintext (utils/decrypt-single-byte-XOR-encrypted-ciphertext s)))

(defn challenge-four
  []
  (let [url "http://cryptopals.com/static/challenge-data/4.txt"
        ciphertexts (clojure.string/split-lines (slurp url))]
    (->> (keep utils/decrypt-single-byte-XOR-encrypted-ciphertext ciphertexts)
         (sort-by :score >)
         (first)
         (:plaintext))))


(defn challenge-five
  [k s]
  (utils/encrypt-with-repeating-XOR-cipher k s))


#_(defn challenge-six
  []
  (let [url "http://cryptopals.com/static/challenge-data/6.txt"
        bs (utils/read-base64-string (clojure.string/replace (slurp url) #"\n" ""))
        key-sizes (range 2 41)
        block-analysis (for [key-size key-sizes]
                         (let [blocks (take (* 2 key-size) bs)
                               block-a (take key-size blocks)
                               block-b (drop key-size blocks)
                               hamming-distance (float (/ (utils/hamming-distance (byte-array block-a)
                                                                                  (byte-array block-b))
                                                          key-size))]
                           {:key-size key-size :hamming-distance hamming-distance}))
        most-likey-key-size (:key-size (first (sort-by :hamming-distance block-analysis)))]
    (for [i (range most-likey-key-size)]
      (keep #(nth % i nil) (partition-all most-likey-key-size bs)))))

#_(challenge-six)

