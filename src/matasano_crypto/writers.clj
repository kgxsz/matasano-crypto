(ns matasano-crypto.writers
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn write-binary-string
  "Takes a byte-array and returns the binary string representation."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/binary-string %)]}
  (let [zero-pad (fn [s] (clojure.string/replace (format "%8s" s) #" " "0"))
        octet-to-binary-string (fn [o] (Integer/toString (cond-> o (neg? o) (+ 256)) 2))]
    (->> (map octet-to-binary-string bs)
         (map zero-pad)
         (apply str))))


(defn write-hex-string
  "Takes a byte-array and returns the hex string representation."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/hex-string %)]}
  (let [zero-pad (fn [s] (clojure.string/replace (format "%2s" s) #" " "0"))
        octet-to-hex-string (fn [o] (Integer/toString (cond-> o (neg? o) (+ 256)) 16))]
    (->> (map octet-to-hex-string bs)
         (map zero-pad)
         (apply str))))


(defn write-base64-string
  "Takes a byte-array and returns the base64 string representation."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/base64-string %)]}
  (let [octet-group-to-sextet-group (fn [os]
                                      (let [o1 (cond-> (nth os 0) (neg? (nth os 0)) (+ 256))
                                            o2 (cond-> (nth os 1 (byte 0)) (neg? (nth os 1 (byte 0))) (+ 256))
                                            o3 (cond-> (nth os 2 (byte 0)) (neg? (nth os 2 (byte 0))) (+ 256))
                                            s1 (bit-shift-right o1 2)
                                            s2 (bit-or (bit-shift-left (bit-and o1 3) 4)
                                                       (bit-shift-right o2 4))
                                            s3 (bit-or (bit-shift-left (bit-and o2 15) 2)
                                                       (bit-shift-right o3 6))
                                            s4 (bit-and o3 63)]
                                        [s1 s2 s3 s4]))
        sextet-to-base64-string (fn [s]
                                  (let [mapping "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"]
                                    (get mapping s)))
        pad (fn [cs]
              (case (mod (count bs) 3)
                0 cs
                1 (concat (drop-last 2 cs) (repeat 2 \=))
                2 (concat (drop-last cs) [\=])))]
    (->> (partition-all 3 bs)
         (map octet-group-to-sextet-group)
         (flatten)
         (map sextet-to-base64-string)
         (pad)
         (apply str))))


(defn write-ASCII-string
  "Takes a byte-array and returns the corresponding ASCII string."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? string? %)]}
  (apply str (map char bs)))
