;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-lexer-shell
  (:require #?(:clj  [blancas.kern.core :refer [reply failed-empty? unexpected unexpected-input expecting
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
                                  value *tab-width* print-error
                                  run run* parse-file runf runf*
                                  parse-data parse-data-file def-
                                  defn* fwd char-seq f->s member?
                                  get-msg-str make-pos]]
               :cljs [blancas.kern.core :refer [reply failed-empty? unexpected unexpected-input expecting
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
                                  member? get-msg-str make-pos]])
            [blancas.kern.lexer.shell-style :refer [trim lexeme sym new-line one-of none-of token word identifier
                                  field char-lit string-lit
                                  dec-lit oct-lit hex-lit
                                  float-lit bool-lit nil-lit
                                  parens braces angles brackets
                                  semi comma colon dot semi-sep
                                  semi-sep1 comma-sep comma-sep1]]
            [clojure.string :as str]
            #?(:clj  [clojure.test :refer [deftest is testing run-tests]]
               :cljs [cljs.test    :refer [deftest is testing run-tests]]))
  #?(:cljs (:require-macros [blancas.kern.core])))

(defn- re-escape [s]
  #?(:clj  (java.util.regex.Pattern/quote s)
     :cljs (js/RegExp.escape s)))

(defn assert-error
  ([unexpected em]
   (let [pat (re-pattern (str "(?s).*unexpected.+" (re-escape unexpected) ".*"))]
     (is (re-matches pat em))))
  ([unexpected expected em]
   (let [pat (re-pattern (str "(?s).*unexpected.+" (re-escape unexpected)
                              ".*\\sexpecting.+" (re-escape expected) ".*"))]
     (is (re-matches pat em)))))


;; +-------------------------------------------------------------+
;; |                    Shell-style lexers.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (let [s1 (parse (>> trim new-line) "  \t\t\n")]
    (testing "trim - blank, tab, eol, then a new line (separately)"
      (is (= \newline (:value s1)))
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
  (let [s1 (parse (>> (skip trim new-line)  (many1 letter)) " \t\t\nABC")]
    (testing "trim - some whitespace before letters"
      (is (= [\A \B \C] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0015
  (let [s1 (parse (<*> (lexeme (sym* \space)) new-line eof) "  \t\t\n")]
    (testing "lexeme - a blank, then tab, eol; then eof"
      (is (= [\space \newline nil] (:value s1)))
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
  (let [s1 (parse (lexeme (many1 letter)) "ABC \t\t\t")]
    (testing "lexeme - some whitespace after letters"
      (is (= [\A \B \C] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0030
  (let [s1 (parse new-line "\nfoo")]
    (testing "new-line - parses a new line and stops"
      (is (= \newline (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0035
  (let [s1 (parse new-line "\n\t\t   foo")]
    (testing "new-line - skip a new line and any other whitespace that follows"
      (is (= \newline (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0040
  (let [s1 (parse new-line "\r\nfoo")]
    (testing "new-line - parses a Windows new-line and stops"
      (is (= \newline (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (= [\f \o \o] (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0045
  (let [s1 (parse new-line "foo")
    	em (get-msg-str (:error s1))]
    (testing "new-line - fails when there's no new-line"
      (is (= [\f \o \o] (:input s1)))
      (is (= nil (:value s1)))
      (is (= false (:ok    s1)))
      (is (= true (:empty s1)))
      (assert-error "f" "new line" em))))


(deftest test-0050
  (let [s1 (parse (lexeme (many digit)) "123 # and the rest is history...")]
    (testing "lexeme - skip over a line comment"
      (is (= [\1 \2 \3] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0055
  (let [s1 (parse identifier "init.phase-123_last=")]
    (testing "lexeme - identifier with - and ."
      (is (= "init.phase-123_last" (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (= [\=] (:input s1)))
      (is (= false (:empty s1))))))


(deftest test-0060
  (let [s1 (parse (many1 identifier) "abc def ghi \\\nxyz")]
    (testing "lexeme - line continuation; no new line to skip"
      (is (= ["abc" "def" "ghi" "xyz"] (:value s1)))
      (is (= true (:ok    s1)))
      (is (= nil (:error s1)))
      (is (empty? (:input s1)))
      (is (= false (:empty s1))))))
