(ns matasano-crypto.utils-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.utils :as utils]))


(deftest test-byte-string
  (testing "it returns a byte string when given a byte"
    (is (= "00101010" (utils/byte-string (byte 42)))))

  (testing "it throws an assertion error when not given a byte"
    (is (thrown? java.lang.AssertionError (utils/byte-string "42")))))


(deftest test-read-hex-char
  (testing "it returns a byte corresponding to the hex character"
    (is (= (byte 15) (utils/read-hex-char \F))))

  (testing "it is case insensitive"
    (is (= (byte 10) (utils/read-hex-char \a))))

  (testing "it throws a number format exception when the char is not valid"
    (is (thrown? java.lang.NumberFormatException (utils/read-hex-char \k))))

  (testing "it throws an assertion error when not given a char"
    (is (thrown? java.lang.AssertionError (utils/read-hex-char "4")))))


(deftest test-read-hex-string
  (testing "it returns a collection of bytes corresponding to the hex string"
    (is (= (list (byte 15) (byte 5)) (utils/read-hex-string "F5"))))

  (testing "it is case insensitive"
    (is (= (list (byte 15) (byte 5)) (utils/read-hex-string "f5"))))

  (testing "it throws a number format exception when part of the string is not valid"
    (is (thrown? java.lang.NumberFormatException (utils/read-hex-string "Fk"))))

  (testing "it throws an assertion error when not given a string"
    (is (thrown? java.lang.AssertionError (utils/read-hex-string 5)))))
