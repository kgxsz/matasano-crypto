(ns matasano-crypto.utils)


(defn print-bits
  [b]
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
