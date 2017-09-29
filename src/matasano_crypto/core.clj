(ns matasano-crypto.core
  (:require [matasano-crypto.utils :as utils]))


(defn challenge-one
  [s]
  (utils/write-base64-string (utils/read-even-hex-string s)))

