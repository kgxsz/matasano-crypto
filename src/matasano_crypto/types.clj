(ns matasano-crypto.types
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::byte (partial instance? java.lang.Byte))
