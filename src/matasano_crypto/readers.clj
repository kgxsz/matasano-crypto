(ns matasano-crypto.readers
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn read-binary-string
  "Takes a binary string and returns the corresponding byte-array."
  [s]
  {:pre [(spec/valid? ::types/binary-string s)]
   :post [(spec/valid? ::types/bytes %)]}
  (let [chars-to-octet (fn [cs]
                         (let [i (Integer/parseInt (apply str cs) 2)]
                           (byte (cond-> i (> i 127) (- 256)))))]
    (->> (partition 8 s)
         (map chars-to-octet)
         (byte-array))))


(defn read-hex-string
  "Takes a hex string and returns the corresponding byte-array."
  [s]
  {:pre [(spec/valid? ::types/hex-string s)]
   :post [(spec/valid? ::types/bytes %)]}
  (let [chars-to-octet (fn [cs]
                         (let [i (Integer/parseInt (apply str cs) 16)]
                           (byte (cond-> i (> i 127) (- 256)))))]
    (->> (partition 2 s)
         (map chars-to-octet)
         (byte-array))))
