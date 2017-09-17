(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn byte-string
  "Takes a byte and returns its string representation, useful for inspection and printing."
  [b]
  {:pre [(spec/valid? ::types/byte b)]
   :post [(spec/valid? ::types/byte-string %)]}
  (->> (range 8)
       (map #(bit-test b %))
       (reverse)
       (map #(if % "1" "0"))
       (apply str)))


(defn read-hex-char
  "Takes a hex character and returns the corresponding byte."
  [c]
  {:pre [(spec/valid? char? c)]
   :post [(spec/valid? ::types/byte %)]}
  (byte (Integer/parseInt (str c) 16)))


(defn read-hex-string
  ;; TODO - complete tests and add some documentation
  [s]
  (map read-hex-char (seq s)))
