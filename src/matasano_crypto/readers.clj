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


(defn read-base64-string
  "Takes a base64 string and returns the corresponding byte-array."
  [s]
  {:pre [(spec/valid? ::types/base64-string s)]
   :post [(spec/valid? ::types/bytes %)]}
  (let [char-to-sextet (fn [c]
                         (let [mapping "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"]
                           (byte (or (clojure.string/index-of mapping c) 0))))
        sextet-group-to-octet-group (fn [[s1 s2 s3 s4]]
                                      (let [o1 (bit-or (bit-shift-left (bit-and s1 63) 2)
                                                       (bit-shift-right s2 4))
                                            o2 (bit-or (bit-shift-left (bit-and s2 15) 4)
                                                       (bit-shift-right (bit-and s3 15) 2))
                                            o3 (bit-or (bit-shift-left (bit-and s3 3) 6)
                                                       (bit-and s4 63))]
                                        [(byte (cond-> o1 (> o1 127) (- 256)))
                                         (byte (cond-> o2 (> o2 127) (- 256)))
                                         (byte (cond-> o3 (> o3 127) (- 256)))]))
        unpad (fn [os]
                (cond
                  (= '(\= \=) (take-last 2 s)) (drop-last 2 os)
                  (= \= (last s)) (butlast os)
                  :else os))]
    (->> (map char-to-sextet s)
         (partition 4)
         (map sextet-group-to-octet-group)
         (flatten)
         (unpad)
         (byte-array))))


(defn read-ASCII-string
  "Takes an ASCII string and returns the corresponding byte-array."
  [s]
  {:pre [(spec/valid? string? s)]
   :post [(spec/valid? ::types/bytes %)]}
  (byte-array (map byte s)))
