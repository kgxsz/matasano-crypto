(ns matasano-crypto.core
  (:require [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (utils/write-base64-string (utils/read-even-hex-string s)))

(defn challenge-two
  [s1 s2]
  (utils/write-hex-string (utils/fixed-XOR (utils/read-even-hex-string s1) (utils/read-even-hex-string s2))))
