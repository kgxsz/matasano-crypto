(ns matasano-crypto.types
  (:require [clojure.spec.alpha :as spec]))

(def binary-string-regex #"^[0|1]*$")

(def hex-string-regex #"^[0-9a-fA-F]*$")

(def base64-string-regex #"^[A-Za-z0-9+/]*={0,2}$")

(def non-zero-length-string-regex #"^[.\S\s]+$")

(defn divisible-by? [n] #(zero? (mod (count %) n)))

(spec/def ::byte (partial instance? java.lang.Byte))

(spec/def ::bytes (partial instance? (Class/forName "[B")))

(spec/def ::binary-string (spec/and string? (partial re-matches binary-string-regex) (divisible-by? 8)))

(spec/def ::even-hex-string (spec/and string? (partial re-matches hex-string-regex) (divisible-by? 2)))

(spec/def ::partitioned-hex-string (spec/and string? (partial re-matches hex-string-regex) #(= 2 (count %))))

(spec/def ::base64-string (spec/and string? (partial re-matches base64-string-regex) (divisible-by? 4)))

(spec/def ::non-zero-length-string (spec/and string? (partial re-matches non-zero-length-string-regex)))

(spec/def ::plaintext ::non-zero-length-string)

(spec/def ::score int?)

(spec/def ::decrypted-cipher (spec/or :candidate-does-not-exist nil?
                                      :candidate-exists (spec/keys :req-un [::plaintext ::score])))

