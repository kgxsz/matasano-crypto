(ns matasano-crypto.utils-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.utils :as utils]))


(deftest test-byte-string
  (testing "it returns a byte string when given a byte"
    (is (= "00101010" (utils/byte-string (byte 42)))))

  (testing "it throws an assertion error when not given a byte"
    (is (thrown? java.lang.AssertionError (utils/byte-string "42")))))
