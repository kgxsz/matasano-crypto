(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn byte-string
  ;; TODO - complete tests and add some documentation
  [b]
  {:pre [(spec/valid? ::types/byte b)]
   :post [(spec/valid? ::types/byte-string %)]}
  (->> (range 8)
       (map #(bit-test b %))
       (reverse)
       (map #(if % "1" "0"))
       (apply str)))


(defn read-hex-char
  ;; TODO - complete tests and add some documentation
  [c]
  (byte (Integer/parseInt (str c) 16)))


(defn read-hex-string
  ;; TODO - complete tests and add some documentation
  [s]
  (map read-hex-char (seq s)))
