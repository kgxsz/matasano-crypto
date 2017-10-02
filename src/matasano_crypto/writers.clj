(ns matasano-crypto.writers
  (:require [matasano-crypto.types :as types]
            [clojure.spec.alpha :as spec]))


(defn write-binary-string
  "Takes a byte-array and returns the binary string representation."
  [bs]
  {:pre [(spec/valid? ::types/bytes bs)]
   :post [(spec/valid? ::types/binary-string %)]}
  (let [zero-pad (fn [s] (clojure.string/replace (format "%8s" s) #" " "0"))
        octet-to-binary-string (fn [o] (Integer/toString (cond-> o (neg? o) (+ 256)) 2))]
    (->> (map octet-to-binary-string bs)
         (map zero-pad)
         (apply str))))
