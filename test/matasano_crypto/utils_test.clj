(ns matasano-crypto.utils-test
  (:require [clojure.test :refer :all]
            [matasano-crypto.utils :as utils]))


(deftest test-byte-to-string
  (testing "it returns a binary string when given a byte"
    (is (= "00101010"
           (utils/byte-to-string (byte 42)))))

  (testing "it throws an assertion error when not given a byte"
    (is (thrown? java.lang.AssertionError (utils/byte-to-string "42")))))


(deftest test-bytes-to-string
  (testing "it returns a binary string when given a byte-array"
    (is (= "0010101000000001"
           (utils/bytes-to-string (byte-array [(byte 42) (byte 1)])))))

  (testing "it throws an assertion error when not given a byte-array"
    (is (thrown? java.lang.AssertionError (utils/bytes-to-string "42")))))


(deftest test-read-partitioned-hex-string
  (testing "it returns a byte corresponding to the hex string"
    (is (= (byte 127) (utils/read-partitioned-hex-string "7F"))))

  (testing "it uses the full byte and ignores two's compliment"
    (is (= (byte -1) (utils/read-partitioned-hex-string "FF"))))

  (testing "it is case insensitive"
    (is (= (byte -1) (utils/read-partitioned-hex-string "Ff"))))

  (testing "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string 42))))

  (testing "it throws an assertion error when the input is not a string of length two"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000"))))

  (testing "it throws an assertion error when the input contains invalid hex"
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "000")))
    (is (thrown? java.lang.AssertionError (utils/read-partitioned-hex-string "0G")))))


(deftest test-read-even-hex-string
  (testing "it returns a byte-array corresponding to the hex string"
    (is (= (vec (byte-array [(byte 73) (byte 39) (byte 109)]))
           (vec (utils/read-even-hex-string "49276d")))))

  (testing  "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/read-even-hex-string 5)))))


(deftest test-write-hex-string
  (testing "it returns a hex string when given a byte-array"
    (is (= "807f01"
           (utils/write-hex-string (byte-array [(byte -128) (byte 127) (byte 1)])))))

  (testing  "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/write-hex-string 5)))))


(deftest test-write-base64-string
  (testing "it returns a base64 string when given a byte-array"
    (is (= "TWFu"
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110)])))))

  (testing "it adds padding appropriately"
    (is (= "TWFuAQ=="
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1)]))))
    (is (= "TWFuAQE="
           (utils/write-base64-string (byte-array [(byte 77) (byte 97) (byte 110) (byte 1) (byte 1)])))))

  (testing "it throws an assertion error when the input is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/write-base64-string (byte 77))))))


(deftest test-fixed-XOR
  (testing "it returns an XOR'd byte-array when given two equal length byte-arrays"
    (is (= (vec (byte-array [(byte -1) (byte 15)]))
           (vec (utils/fixed-XOR (byte-array [(byte -16) (byte -1)]) (byte-array [(byte 15) (byte -16)]))))))

  (testing "it throws an assertion error when either of the inputs are not byte-arrays"
    (is (thrown? java.lang.AssertionError (utils/fixed-XOR (byte-array []) 42)))
    (is (thrown? java.lang.AssertionError (utils/fixed-XOR 42 (byte-array [])))))

  (testing "it throws an assertion error when the input byte-arrays are not equal length"
    (is (thrown? java.lang.AssertionError (utils/fixed-XOR (byte-array [(byte -16)]) (byte-array [(byte 15) (byte -16)]))))))


(deftest test-write-plaintext-string
  (testing "it returns a plaintext string when given a byte-array"
    (is (= "abc"
           (utils/write-plaintext-string (byte-array [(byte 97) (byte 98) (byte 99)])))))

  (testing "it throws an illegal argument exception when a byte is out of range"
    (is (thrown? java.lang.IllegalArgumentException (utils/write-plaintext-string (byte-array [(byte -1)])))))

  (testing "it throws an assertion error when the input is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/write-plaintext-string (byte 77))))))


(deftest test-score-plaintext-string
  (testing "it returns a score based on the string"
    (is (= 1
           (utils/score-plaintext "xyzd':\";bmmsa**")))
    (is (= 5
           (utils/score-plaintext "It was the best of times it was the worst of times."))))

  (testing "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/score-plaintext 42)))))


(deftest test-decrypt-single-byte-XOR-encrypted-ciphertext
  (testing "it returns the most likely plaintext along with the score and value based on the string"
    (is (= {:plaintext "Cooking MC's like a pound of bacon" :score 5}
           (utils/decrypt-single-byte-XOR-encrypted-ciphertext "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))))

  (testing "it returns nil when there are no possible candidates"
    (is (nil? (utils/decrypt-single-byte-XOR-encrypted-ciphertext "0e3647e8592d35514a081243582536ed3de6734059001e3f535ce6271032"))))

  (testing "it throws an assertion error when the input is not a string"
    (is (thrown? java.lang.AssertionError (utils/decrypt-single-byte-XOR-encrypted-ciphertext 42)))))


(deftest test-encrypt-with-repeating-XOR-cipher
  (testing "it returns the correct ciphertext given a key and plaintext"
    (is (= "09070d0e0e"
           (utils/encrypt-with-repeating-XOR-cipher "ab" "hello"))))

  (testing "it throws an assertion error when either of the inputs is a zero length string"
    (is (thrown? java.lang.AssertionError (utils/encrypt-with-repeating-XOR-cipher "a" "")))
    (is (thrown? java.lang.AssertionError (utils/encrypt-with-repeating-XOR-cipher "" "a")))
    )

  (testing "it throws an assertion error when either of the inputs is not a string"
    (is (thrown? java.lang.AssertionError (utils/encrypt-with-repeating-XOR-cipher "a" 42)))
    (is (thrown? java.lang.AssertionError (utils/encrypt-with-repeating-XOR-cipher 42 "a")))
    ))
