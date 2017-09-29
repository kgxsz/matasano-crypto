(ns matasano-crypto.core
  (:require [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (utils/write-base64-string (utils/read-even-hex-string s)))

(defn challenge-two
  [s1 s2]
  "746865206b696420646f6e277420706c6179"
  )
