(ns matasano-crypto.core
  (:require [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (utils/write-base64-string (utils/read-even-hex-string s)))


(defn challenge-two
  [s1 s2]
  (utils/write-hex-string (utils/fixed-XOR (utils/read-even-hex-string s1) (utils/read-even-hex-string s2))))


(defn challenge-three
  [s]
  (let [bs (utils/read-even-hex-string s)
        make-cipher (fn [n] (byte-array (repeat (count bs) (byte n))))
        apply-cipher (fn [c] (utils/fixed-XOR bs c))
        contains-unreadable-characters? (fn [d] (not-every? #(<= 32 % 126) (vec d)))
        apply-score (fn [d] (let [s (utils/write-plaintext-string d)]
                              {:score (utils/score-plaintext s) :value s}))]
    (->> (range -128 128)
         (map make-cipher)
         (map apply-cipher)
         (remove contains-unreadable-characters?)
         (map apply-score)
         (sort-by :score >)
         (first)
         (:value))))
