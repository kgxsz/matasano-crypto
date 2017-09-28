(ns matasano-crypto.types
  (:require [clojure.spec.alpha :as spec]))

(def binary-string-regex #"^[0|1]+$")

(def hex-string-regex #"^[0-9a-fA-F]*$")

(spec/def ::byte (partial instance? java.lang.Byte))

(spec/def ::bytes (partial instance? (Class/forName "[B")))

(spec/def ::binary-string (spec/and string? (partial re-matches binary-string-regex)))

(spec/def ::even-hex-string (spec/and string? (partial re-matches hex-string-regex) (comp even? count)))

(spec/def ::partitioned-hex-string (spec/and string? (partial re-matches hex-string-regex) #(= 2 (count %))))
