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


(defn read-partitioned-hex-string
  "Takes a partitioned hex string consisting of two hex characters and returns the corresponding byte."
  [s]
  {:pre [(spec/valid? ::types/partitioned-hex-string s)]
   :post [(spec/valid? ::types/byte %)]}
  (let [value (Integer/parseInt s 16)
        shifted-value (cond-> value (> value 127) (- 256))]
    (byte shifted-value)))


(defn read-even-hex-string
  "Takes a hex string and returns the corresponding byte-array.
   The hex string must have an even length."
  [s]
  {:pre [(spec/valid? ::types/even-hex-string s)]
   :post [(spec/valid? ::types/bytes %)]}
  (->> (partition 2 s)
       (map (partial apply str))
       (map read-partitioned-hex-string)
       (byte-array)))


(defn write-hex-string
  "Takes a byte-array and returns the hex string representation"
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/even-hex-string %)]}
  (let [octet-to-quartets (fn [octet]
                            (let [quartet-a (bit-and (bit-shift-right octet 4) 15)
                                  quartet-b (bit-and 15 octet)]
                              [quartet-a quartet-b]))
        quartet-to-hex (fn [quartet] (get "0123456789abcdef" quartet))]
    (->> (vec bs)
         (map octet-to-quartets)
         (flatten)
         (map quartet-to-hex)
         (apply str))))


(defn read-base64-string
  "Takes a base64 string and returns the corresponding byte-array."
  [s]
  {:pre [(spec/valid? ::types/base64-string s)]
   :post [(spec/valid? ::types/bytes %)]}
  (let [base64-to-sextet (fn [c]
                           (or (clojure.string/index-of
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
                                c)
                               0))
        sextets-to-octets (fn [[sextet-a sextet-b sextet-c sextet-d]]
                            (let [octet-a (bit-or (bit-shift-left (bit-and sextet-a 63) 2)
                                                  (bit-shift-right sextet-b 4))
                                  octet-b (bit-or (bit-shift-left (bit-and sextet-b 15) 4)
                                                  (bit-shift-right (bit-and sextet-c 15) 2))
                                  octet-c (bit-or (bit-shift-left (bit-and sextet-c 3) 6)
                                                  (bit-and sextet-d 63))]
                              [octet-a octet-b octet-c]))
        unpad (fn [ns]
               (cond
                 (= '(\= \=) (take-last 2 s)) (drop-last 2 ns)
                 (= \= (last s)) (butlast ns)
                 :else ns))]
    (->> (map base64-to-sextet s)
         (partition-all 4)
         (map sextets-to-octets)
         (flatten)
         (unpad)
         (map byte)
         (byte-array))))


(defn write-base64-string
  "Takes a byte-array and returns the base64 string representation"
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/base64-string %)]}
  (let [octets-to-sextets (fn [octets]
                            (let [octet-a (nth octets 0)
                                  octet-b (nth octets 1 (byte 0))
                                  octet-c (nth octets 2 (byte 0))
                                  sextet-a (bit-shift-right octet-a 2)
                                  sextet-b (bit-or (bit-shift-left (bit-and octet-a 3) 4)
                                                   (bit-shift-right octet-b 4))
                                  sextet-c (bit-or (bit-shift-left (bit-and octet-b 15) 2)
                                                   (bit-shift-right octet-c 6))
                                  sextet-d (bit-and octet-c 63)]
                              [sextet-a sextet-b sextet-c sextet-d]))
        sextet-to-base64 (fn [sextet] (get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" sextet))
        pad (fn [cs]
              (case (mod (count bs) 3)
                0 cs
                1 (concat (drop-last 2 cs) (repeat 2 \=))
                2 (concat (drop-last cs) [\=])))]
    (->> (partition-all 3 bs)
         (map octets-to-sextets)
         (flatten)
         (map sextet-to-base64)
         (pad)
         (apply str))))


(defn fixed-XOR
  "Takes two equal length byte-arrays and performs an XOR operation on them, returns a byte-array of equal length."
  [bs1 bs2]
  {:pre [(spec/valid? ::types/bytes bs1)
         (spec/valid? ::types/bytes bs2)
         (spec/valid? #(= (-> % first count) (-> % second count)) [bs1 bs2])]
   :post [(spec/valid? ::types/bytes %)
          (spec/valid? #(= (count bs1) (count bs2) (count %)) %)]}
  (->> (map (partial bit-xor) bs1 bs2)
       (map byte)
       (byte-array)))


(defn write-plaintext-string
  "Takes a byte-array and returns the corresponding plaintext string."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? string? %)]}
  (apply str (map char bs)))


(defn score-plaintext
  "Takes a plaintext string and assigns a score based on likelihood of the string being valid English.
   The algorithm simply looks at the top 7 most frequent characters in the string and assigns a point
   each time one of those seven characters correspond to one of the seven most frequent characters in
   the English language."
  [s]
  {:pre [(spec/valid? string? s)]
   :post [(spec/valid? int? %)]}
  (let [frequent-char? (fn [c] (contains? #{\e \t \a \o \i \n \space} c))]
    (->> (clojure.string/lower-case s)
         (frequencies)
         (sort-by second >)
         (take 7)
         (keys)
         (map frequent-char?)
         (filter true?)
         (count))))


(defn decrypt-single-byte-XOR-encrypted-ciphertext
  "Takes ciphertext that was encrypted with a single byte XOR cipher and return the most likely decryption."
  [s]
  {:pre [(spec/valid? string? s)]
   :post [(spec/valid? ::types/decrypted-cipher %)]}
  (let [bs (read-even-hex-string s)
        make-cipher (fn [n] (byte-array (repeat (count bs) (byte n))))
        apply-cipher (fn [c] (fixed-XOR bs c))
        contains-unreadable-characters? (fn [d] (not-every? #(<= 0 % 127) (vec d)))
        apply-score (fn [d]
                      (let [s (write-plaintext-string d)]
                        {:score (score-plaintext s) :plaintext s}))]
    (->> (range -128 128)
         (map make-cipher)
         (map apply-cipher)
         (remove contains-unreadable-characters?)
         (map apply-score)
         (sort-by :score >)
         (first))))


(defn encrypt-with-repeating-XOR-cipher
  "Takes a key and string, encrypts the string with a repeating XOR cipher using the key."
  [k s]
  {:pre [(spec/valid? ::types/non-zero-length-string k)
         (spec/valid? ::types/non-zero-length-string s)]
   :post [(spec/valid? ::types/non-zero-length-string %)]}
  (let [bs1 (byte-array (map byte s))
        bs2 (byte-array (map byte (take (count s) (cycle k))))]
    (write-hex-string (fixed-XOR bs1 bs2))))


(defn hamming-distance
  "Computes the hamming distance between two byte-arrays of equal length."
  [bs1 bs2]
  {:pre [(spec/valid? ::types/bytes bs1)
         (spec/valid? ::types/bytes bs2)
         (spec/valid? #(= (-> % first count) (-> % second count)) [bs1 bs2])]
   :post [(spec/valid? int? %)]}
  (let [hamming-distance-by-byte (fn [b1 b2]
                                   (let [difference (byte (bit-xor b1 b2))]
                                     (->> (range 8)
                                          (map #(bit-test difference %))
                                          (filter true?)
                                          (count))))]
    (reduce + (map hamming-distance-by-byte bs1 bs2))))
