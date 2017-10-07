(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec])
  (:import (java.security Key)
           (javax.crypto Cipher)
           (javax.crypto.spec SecretKeySpec)))


(defn to-unsigned-byte
  "Converts an integer between 0 and 255 inclusive to an unsigned byte,
   such that two's complement is ignored."
  [i]
  {:pre [(spec/valid? int? i)
         (<= 0 i 255)]
   :post [(spec/valid? ::types/byte %)]}
  (byte (cond-> i (> i 127) (- 256))))


(defn from-unsigned-byte
  "Converts a byte into an integer, ignoring two's complement."
  [b]
  {:pre [(spec/valid? ::types/byte b)]
   :post [(spec/valid? int? %)
          (<= 0 % 255)]}
  (cond-> b (neg? b) (+ 256)))


(defn XOR
  "Takes two equal length byte-arrays and performs a bitwise XOR operation
   on them, returns a byte-array of equal length."
  [bs1 bs2]
  {:pre [(spec/valid? ::types/bytes bs1)
         (spec/valid? ::types/bytes bs2)
         (= (count bs1) (count bs2))]
   :post [(spec/valid? ::types/bytes %)
          (spec/valid? #(= (count bs1) (count bs2) (count %)) %)]}
  (->> (map (partial bit-xor) bs1 bs2)
       (map byte)
       (byte-array)))


(defn score
  "Takes a byte-array and assigns a score based on likelihood that, when
   written as an ASCII string, is valid English. The algorithm simply
   looks at the top seven most frequent bytes and assigns a point each
   time one of those seven bytes correspond to one of the seven most
   frequent characters in the English language. Additionally, frequency
   comparison is made with the aforementioned bytes, so if, for example,
   byte 101 is expected to be more frequent than byte 116, then we assigns
   a point if that is true. Finally, we assign a score of zero if any of
   the bytes corresponds to unprintable characters."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? int? %)]}
  (let [contains-unprintable-bytes? (not-every? #(or (<= 32 % 126) (contains? #{10} %)) bs)
        to-lower-case (fn [b] (cond-> b (<= 65 b 90) (+ 32)))
        character-frequency-table (frequencies (map to-lower-case bs))
        frequent-bytes-appearance-score (->> (sort-by second > character-frequency-table)
                                             (take 7)
                                             (keys)
                                             (map (partial contains? #{32 97 101 105 110 111 116 115 104 114}))
                                             (filter true?)
                                             (count))
        frequent-bytes-order-score (->> [[101 116] [116 97] [97 111] [111 105] [105 110] [110 32] [32 115]]
                                        (map (fn [[a b]]
                                               (> (get character-frequency-table a 0)
                                                  (get character-frequency-table b 0))))
                                        (filter true?)
                                        (count))]
    (if contains-unprintable-bytes?
      0
      (+ frequent-bytes-appearance-score frequent-bytes-order-score))))


(defn hamming-distance
  "Computes the hamming distance between two byte-arrays of equal length."
  [bs1 bs2]
  {:pre [(spec/valid? ::types/bytes bs1)
         (spec/valid? ::types/bytes bs2)
         (= (count bs1) (count bs2))]
   :post [(spec/valid? int? %)]}
  (let [hamming-distance-by-byte (fn [b1 b2]
                                   (let [difference (byte (bit-xor b1 b2))]
                                     (->> (range 8)
                                          (map #(bit-test difference %))
                                          (filter true?)
                                          (count))))]
    (reduce + (map hamming-distance-by-byte bs1 bs2))))


(defn apply-repeating-XOR-cipher
  "Takes a key in the form of a byte-array, along with a byte-array to
   encrypt/decrypt, XORs the byte-array with the repeated key."
  [k bs]
  {:pre [(spec/valid? ::types/bytes k)
         (spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/bytes %)]}
  (XOR bs (byte-array (take (count bs) (cycle k)))))


(defn crack-length-one-key-repeating-XOR-encryption
  "Takes a byte-array that was encrypted with a length one key repeating XOR
   cipher, attempts to crack it by decrypting with a length one key repeating
   XOR cipher for all possible keys, then ranking the results by score."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/cracked-length-one-key-repeating-XOR-encryption %)]}
  (->> (range -128 128)
       (map (fn [n] {:key (byte-array [n]) :value (apply-repeating-XOR-cipher (byte-array [n]) bs)}))
       (map (fn [m] (assoc m :score (score (:value m)))))
       (sort-by :score >)
       (first)))


(defn decrypt-AES-in-ECB-mode
  "Takes a key in the form of a byte-array, along with a byte-array to
   decrypt, performs the AES in ECB mode decryption."
  [k bs]
  {:pre [(spec/valid? ::types/bytes k)
         (spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/bytes %)]}
  (let [cipher (Cipher/getInstance "AES/ECB/PKCS5Padding")]
    (.init cipher Cipher/DECRYPT_MODE (SecretKeySpec. k "AES"))
    (.doFinal cipher bs)))
