(ns matasano-crypto.utils
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


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
