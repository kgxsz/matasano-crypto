(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn byte-to-string
  "Takes a byte and returns its string representation, useful for inspection and printing."
  [b]
  {:pre [(spec/valid? ::types/byte b)]
   :post [(spec/valid? ::types/binary-string %)]}
  (->> (range 8)
       (map #(bit-test b %))
       (reverse)
       (map #(if % "1" "0"))
       (apply str)))


(defn bytes-to-string
  "Takes a byte-array and returns its string representation, useful for inspection and printing."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/binary-string %)]}
  (apply str (map byte-to-string bs)))


(defn read-hex-char
  "Takes a hex character and returns the corresponding decimal integer."
  [c]
  {:pre [(spec/valid? char? c)]
   :post [(spec/valid? int? %)]}
  (Integer/parseInt (str c) 16))


(defn read-partitioned-hex-string
  "Takes a partitioned hex string consisting of two hex characters and returns the corresponding byte."
  [s]
  {:pre [(spec/valid? ::types/partitioned-hex-string s)]
   :post [(spec/valid? ::types/byte %)]}
  (let [value (Integer/parseInt s 16)
        shifted-value (cond-> value (> value 127) (- 256))]
    (byte shifted-value)))


(defn read-hex-string
  "Takes a hex string and returns the corresponding collection of bytes."
  [cs]
  {:pre [(spec/valid? string? cs)]
   :post [(spec/valid? ::types/bytes %)]}
  (->> (seq cs)
       (map (comp byte read-hex-char))
       (byte-array)))
