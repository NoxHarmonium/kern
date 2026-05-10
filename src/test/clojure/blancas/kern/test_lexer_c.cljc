;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-c
  (:require [blancas.kern.core :refer [reply failed-empty? unexpected unexpected-input expecting
                                  clear-empty return fail satisfy
                                  <?> expect <|> >>= bind >> <<
                                  <$> <*> <:> many many0 many1
                                  optional option skip skip-many
                                  skip-many1 sep-by1 sep-by end-by
                                  end-by1 sep-end-by1 sep-end-by
                                  between times look-ahead predict
                                  not-followed-by many-till <+>
                                  search any-char letter lower
                                  upper white-space space tab
                                  digit hex-digit oct-digit
                                  alpha-num sym* sym- token*
                                  token- word* word- one-of*
                                  none-of* new-line* eof skip-ws
                                  field* split-on split mark
                                  dec-num oct-num hex-num
                                  float-num get-state put-state
                                  modify-state get-input set-input
                                  get-position set-position parse
                                  value parse-data def- defn* fwd
                                  member? get-msg-str make-pos]]
            [blancas.kern.lexer.c-style :refer [trim lexeme sym new-line one-of none-of token word identifier
                                  field char-lit string-lit
                                  dec-lit oct-lit hex-lit
                                  float-lit bool-lit nil-lit
                                  parens braces angles brackets
                                  semi comma colon dot semi-sep
                                  semi-sep1 comma-sep comma-sep1]]
            [blancas.kern.test-helpers :refer [assert-error space-string]]
            [clojure.string :as str]
            #?(:clj  [clojure.test :refer [deftest is testing run-tests]]
               :cljs [cljs.test    :refer [deftest is testing run-tests]]))
  #?(:cljs (:require-macros [blancas.kern.core])))

;; +-------------------------------------------------------------+
;; |                    Java-style lexers.                       |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> trim eof) "  \t\t\n")]
    (testing "trim - blank, tab, eol, then eof"
      (is (= nil (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0005
  (let [s1 (parse (>> trim (many digit)) "123")]
    (testing "trim - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0010
  (let [s1 (parse (>> trim (many1 letter)) "  /* comment */ \t\n\t\t ABC")]
    (testing "trim - whitespace before letters"
      (is (= [\A \B \C] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0015
  (let [s1 (parse (>> (lexeme (sym* \space)) eof) "  \t\t\n")]
    (testing "lexeme - a blank, then tab, eol; then eof"
      (is (= nil (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0020
  (let [s1 (parse (lexeme (many digit)) "123")]
    (testing "lexeme - no whitespace, it's optional"
      (is (= [\1 \2 \3] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0025
  (let [s1 (parse (lexeme (many1 letter)) "ABC /* that's it */ \t\n\t\t")]
    (testing "lexeme - whitespace and comments after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0030
  (let [s1 (parse (lexeme (many1 letter)) "foo // and the rest is history\nbar")]
    (testing "trim - single-line comment"
      (is (= [\f \o \o] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (= [\b \a \r] (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0035
  (let [in "foo // variable\n// that's all\n// for now\nbar"
	s1 (parse (lexeme (many1 letter)) in)]
    (testing "lexeme - skips over multiple single-line comments"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1)))
      (is (= nil (:error s1))))))


(deftest test-0040
  (let [in "foo /* var foo\n  that's all\n for now */\nbar"
        s1 (parse (lexeme (many1 letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (= [\b \a \r] (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1)))
      (is (= nil (:error s1))))))


(deftest test-0045
  (let [in "foo/********this is a comment**********/"
	s1 (parse (lexeme (many1 letter)) in)]
    (testing "lexeme - skips over multiple multi-line comment"
      (is (empty? (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1)))
      (is (= nil (:error s1))))))


(deftest test-0050
  (let [in "foo/****** this is a comment*****"
	s1 (parse (lexeme (many1 letter)) in)
    	em (get-msg-str (:error s1))]
    (testing "lexeme - fails looking for end of comment"
      (is (empty? (:input s1)))
      (is (= nil (:value s1)))
      (is (= false (:ok    s1)))
      (is (= false (:empty s1)))
      (assert-error "end of input" "end of comment" em))))


(deftest test-0055
  (let [in "foo/******* this is a /* CAN I NEST? */ comment ********/"
	s1 (parse (lexeme (many1 letter)) in)]
    (testing "lexeme - Won't da nested comment; but this works and stops at 'comment'"
      (is (= (seq "comment ********/") (:input s1)))
      (is (= [\f \o \o] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0060
  (let [s1 (parse char-lit "'z'")]
    (testing "char-lit"
      (is (= \z (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0065
  (let [s1 (parse char-lit "'\\b'")]
    (testing "char-lit"
      (is (= \backspace (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0070
  (let [s1 (parse char-lit "'\\t'")]
    (testing "char-lit"
      (is (= \tab (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0075
  (let [s1 (parse char-lit "'\\n'")]
    (testing "char-lit"
      (is (= \newline (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0080
  (let [s1 (parse char-lit "'\\f'")]
    (testing "char-lit"
      (is (= \formfeed (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0085
  (let [s1 (parse char-lit "'\\r'")]
    (testing "char-lit"
      (is (= \return (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0090
  (let [s1 (parse char-lit "'\\''")]
    (testing "char-lit"
      (is (= \' (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0095
  (let [s1 (parse char-lit "'\\\"'")]
    (testing "char-lit"
      (is (= \" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0100
  (let [s1 (parse char-lit "'\\\\'")]
    (testing "char-lit"
      (is (= \\ (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0105
  (let [s1 (parse char-lit "'\\/'")]
    (testing "char-lit"
      (is (= \/ (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0110
  (let [s1 (parse char-lit "'\\u0041'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0115
  (let [s1 (parse char-lit "'\\101'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0120
  (let [in "'a "
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (testing "char-lit - fails without the closing quote"
      (is (= [\space] (:input s1)))
      (is (= nil (:value s1)))
      (is (= false (:ok    s1)))
      (is (= false (:empty s1)))
      (assert-error space-string "end of character literal" em))))


(deftest test-0125
  (let [in "'\\u2'"
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (testing "char-lit - fails: incomplete unicode number"
      (is (= [\'] (:input s1)))
      (is (= nil (:value s1)))
      (is (= false (:ok    s1)))
      (is (= false (:empty s1)))
      (assert-error "'" "hexadecimal digit" em))))


(deftest test-0130
  (let [in "'\\00*"
	s1 (parse char-lit in)
    	em (get-msg-str (:error s1))]
    (testing "char-lit - fails: octal numbers are assumed; that's acually char \0"
      (is (= [\0 \*] (:input s1)))
      (is (= nil (:value s1)))
      (is (= false (:ok    s1)))
      (is (= false (:empty s1)))
      (assert-error "0" "end of character literal" em))))


(deftest test-0135
  (let [s1 (parse char-lit "'\\?'")]
    (testing "char-lit"
      (is (= \? (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0140
  (let [s1 (parse char-lit "'\\a'")]
    (testing "char-lit"
      (is (= (char 7) (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0145
  (let [s1 (parse char-lit "'\\v'")]
    (testing "char-lit"
      (is (= (char 11) (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0150
  (let [s1 (parse char-lit "'\\0'")]
    (testing "char-lit"
      (is (= (char 0) (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0155
  (let [s1 (parse char-lit "'\\x41'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0160
  (let [s1 (parse char-lit "'\\U00000041'")]
    (testing "char-lit"
      (is (= \A (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0165
  (let [in "\"\\bnow\\tis\\nthe\\ftime\\r\" \t\t|;"
	s1 (parse string-lit in)]
    (testing "string-lit - parses a simple string literal"
      (is (= [\| \;] (:input s1)))
      (is (= "\bnow\tis\nthe\ftime\r" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0170
  (let [in "\"null-terminated\\0\";"
	s1 (parse string-lit in)]
    (testing "string-lit - parses a null-terminated string"
      (is (= [\;] (:input s1)))
      (is (= "null-terminated\u0000" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0175
  (let [in "\"\\\\null-terminated\\?\";"
	s1 (parse string-lit in)]
    (testing "string-lit - parses a backslash and a question mark"
      (is (= [\;] (:input s1)))
      (is (= "\\null-terminated?" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0180
  (let [in "\"\\tnow is \\u0074\\u0068\\u0065 time\" \t\t|;"
	s1 (parse string-lit in)]
    (testing "string-lit - parses unicode characters"
      (is (= [\| \;] (:input s1)))
      (is (= "\tnow is the time" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0185
  (let [in "\"now is \\164\\150\\145 time\" /* the */|;"
	s1 (parse string-lit in)]
    (testing "string-lit - parses octal characters"
      (is (= [\| \;] (:input s1)))
      (is (= "now is the time" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))


(deftest test-0190
  (let [in "\"now is \\x74\\x68\\x65 time\" \t\t|;"
	s1 (parse string-lit in)]
    (testing "string-lit - parses hex characters"
      (is (= [\| \;] (:input s1)))
      (is (= "now is the time" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= false (:empty s1))))))
