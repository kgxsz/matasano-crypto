(ns matasano-crypto.types
  (:require [clojure.spec.alpha :as spec]))

(def binary-string-regex #"^[0|1]+$")

(def partitioned-hex-string-regex #"^[0-9a-fA-F]{2}$")

(spec/def ::byte (partial instance? java.lang.Byte))

(spec/def ::bytes (partial instance? (Class/forName "[B")))

(spec/def ::binary-string (spec/and string? (partial re-matches binary-string-regex)))

(spec/def ::partitioned-hex-string (spec/and string? (partial re-matches partitioned-hex-string-regex)))
