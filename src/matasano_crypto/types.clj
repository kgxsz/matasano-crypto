(ns matasano-crypto.types
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::byte (partial instance? java.lang.Byte))

(spec/def ::byte-string (spec/and string? #(= 8 (count %))))
