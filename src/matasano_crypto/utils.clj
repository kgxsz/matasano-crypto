(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))

(defn print-bits
  [b]
  {:pre [(spec/valid? ::types/byte b)]
   :post [(spec/valid? string? %)]}
  (let [bits (->> (range 8)
                  (map #(bit-test b %))
                  (reverse)
                  (map #(if (true? %) "1" "0"))
                  (apply str))]
    bits))


(defn read-hex-char
  [c]
  (byte (Integer/parseInt (str c) 16)))


(defn read-hex-string
  [s]
  (map read-hex-char (seq s)))
