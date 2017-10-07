(ns matasano-crypto.utils-test
  (:require [matasano-crypto.readers :as readers]
            [matasano-crypto.utils :as utils]
            [matasano-crypto.writers :as writers]
            [clojure.test :refer :all]))


(deftest test-XOR
  (testing "it returns an XOR'd byte-array when given two equal length byte-arrays"
    (is (= (vec (readers/read-binary-string "0000000011111111"))
           (vec (utils/XOR (readers/read-binary-string "1111111010101010")
                           (readers/read-binary-string "1111111001010101"))))))

  (testing "it throws an assertion error when either of the inputs are not byte-arrays"
    (is (thrown? java.lang.AssertionError (utils/XOR (readers/read-hex-string "42")
                                                     42)))
    (is (thrown? java.lang.AssertionError (utils/XOR 42
                                                     (readers/read-hex-string "42")))))

  (testing "it throws an assertion error when the input byte-arrays are not equal length"
    (is (thrown? java.lang.AssertionError (utils/XOR (readers/read-hex-string "42")
                                                     (readers/read-hex-string "42ff"))))))


(deftest test-score
  (testing "it returns a score"
    (is (= 1
           (utils/score (readers/read-ASCII-string "xyzd':\";bmmsa**"))))
    (is (= 8
           (utils/score (readers/read-ASCII-string "hello world")))))

  (testing "it ignores case when scoring"
    (is (= 8
           (utils/score (readers/read-ASCII-string "HeLlo wOrLd")))))

  (testing "it gives a zero score when infrequent control characters are present"
    (= 0
       (utils/score (readers/read-ASCII-string "Now that the party is jumping|\n")))
    (= 5
       (utils/score (readers/read-ASCII-string "Now that the party is jumping\n"))))

  (testing "it gives a zero score when unprintable characters are present"
    (is (= 0
           (utils/score (byte-array [104 101 -1 108 111 32 119 111 114 108 100])))))

  (testing "it throws an assertion error when the input is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/score 42)))))


(deftest test-hamming-distance
  (testing "it returns the hamming distance when given two equal length byte-arrays"
    (is (= 37
           (utils/hamming-distance (readers/read-ASCII-string "this is a test")
                                   (readers/read-ASCII-string "wokka wokka!!!")))))

  (testing "it throws an assertion error when either of the inputs is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/hamming-distance (readers/read-hex-string "42")
                                                                  42)))
    (is (thrown? java.lang.AssertionError (utils/hamming-distance 42
                                                                  (readers/read-hex-string "42"))))))


(deftest test-apply-repeating-XOR-cipher
  (testing "it returns an encrypted byte-array given the key"
    (is (= (vec (readers/read-ASCII-string "\t\r\rC\r\r"))
           (vec (utils/apply-repeating-XOR-cipher (readers/read-ASCII-string "abc")
                                                  (readers/read-ASCII-string "hello world"))))))

  (testing "it returns an decrypted byte-array given the key"
    (is (= (vec (readers/read-ASCII-string "hello world"))
           (vec (utils/apply-repeating-XOR-cipher (readers/read-ASCII-string "abc")
                                                  (readers/read-ASCII-string  "\t\r\rC\r\r"))))))

  (testing "it throws an assertion error when either of the inputs is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/hamming-distance (readers/read-hex-string "42")
                                                                  42)))
    (is (thrown? java.lang.AssertionError (utils/hamming-distance 42
                                                                  (readers/read-hex-string "42"))))))


(deftest test-crack-length-one-key-repeating-XOR-encryption
  (testing "it returns the cracked key, score, and value"
    (let [encryption (->> (readers/read-ASCII-string "Cooking MC's like a pound of bacon")
                          (utils/apply-repeating-XOR-cipher (readers/read-ASCII-string "X")))
          {:keys [key score value]} (utils/crack-length-one-key-repeating-XOR-encryption encryption)]
      (is (= "X" (writers/write-ASCII-string key)))
      (is (= 8 score))
      (is (= "Cooking MC's like a pound of bacon" (writers/write-ASCII-string value)))))

  (testing "it throws an assertion error when the input is not a byte-array"
    (is (thrown? java.lang.AssertionError (utils/crack-length-one-key-repeating-XOR-encryption 42)))))
