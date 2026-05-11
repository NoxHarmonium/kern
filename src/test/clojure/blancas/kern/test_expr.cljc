;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-expr
  (:require [blancas.kern.core :refer [parse <|> fwd value]]
            [blancas.kern.expr :refer [chainl1 chainl chainr1 chainr
                                       prefix1 prefix postfix1 postfix
                                       chainl1* chainr1*
                                       add-op mul-op pow-op uni-op
                                       rel-op and-op or-op]]
            [blancas.kern.lexer.c-style :refer [float-lit parens]]
            #?(:clj  [clojure.test :refer [deftest is testing run-tests]]
               :cljs [cljs.test    :refer [deftest is testing run-tests]]))
  #?(:cljs (:require-macros [blancas.kern.core])))

;; +-------------------------------------------------------------+
;; |  A simple infix expression evaluator (from expr.clj)        |
;; |  used as the test subject for blancas.kern.expr.            |
;; +-------------------------------------------------------------+

(declare expr-p)
(def factor  (<|> float-lit (parens (fwd expr-p))))
(def unary   (prefix1 factor  uni-op))
(def power   (chainr1 unary   pow-op))
(def term    (chainl1 power   mul-op))
(def sum     (chainl1 term    add-op))
(def relex   (chainl1 sum     rel-op))
(def orex    (chainl1 relex   or-op))
(def expr-p  (chainl1 orex    and-op))

;; +-------------------------------------------------------------+
;; |                       chainl1 / chainl                      |
;; +-------------------------------------------------------------+

(deftest test-chainl1-addition
  (let [s1 (parse expr-p "3 + 4")]
    (testing "chainl1 - left-associative addition"
      (is (= true (:ok s1)))
      (is (== 7 (:value s1))))))

(deftest test-chainl1-left-assoc
  (let [s1 (parse expr-p "10 - 3 - 2")]
    (testing "chainl1 - subtraction is left-associative: (10-3)-2 = 5, not 10-(3-2) = 9"
      (is (= true (:ok s1)))
      (is (== 5 (:value s1))))))

(deftest test-chainl1-precedence
  (let [s1 (parse expr-p "2 + 3 * 4")]
    (testing "chainl1 - multiplication binds tighter than addition"
      (is (= true (:ok s1)))
      (is (== 14 (:value s1))))))

(deftest test-chainl1-parens
  (let [s1 (parse expr-p "(2 + 3) * 4")]
    (testing "chainl1 - parentheses override precedence"
      (is (= true (:ok s1)))
      (is (== 20 (:value s1))))))

(deftest test-chainl-default
  (let [s1 (parse (chainl sum add-op 99.0) "")]
    (testing "chainl - returns default value when input is empty"
      (is (= true (:ok s1)))
      (is (== 99 (:value s1))))))

;; +-------------------------------------------------------------+
;; |                       chainr1 / chainr                      |
;; +-------------------------------------------------------------+

(deftest test-chainr1-right-assoc
  (let [s1 (parse expr-p "2 ^ 3 ^ 2")]
    (testing "chainr1 - power is right-associative: 2^(3^2) = 512, not (2^3)^2 = 64"
      (is (= true (:ok s1)))
      (is (== 512 (:value s1))))))

(deftest test-chainr-default
  (let [s1 (parse (chainr power pow-op 1.0) "")]
    (testing "chainr - returns default value when input is empty"
      (is (= true (:ok s1)))
      (is (== 1 (:value s1))))))

;; +-------------------------------------------------------------+
;; |                         prefix1 / prefix                    |
;; +-------------------------------------------------------------+

(deftest test-prefix1-unary-minus
  (let [s1 (parse expr-p "-5 + 8")]
    (testing "prefix1 - unary minus applied before addition"
      (is (= true (:ok s1)))
      (is (== 3 (:value s1))))))

(deftest test-prefix1-double-negation
  (let [s1 (parse expr-p "--4")]
    (testing "prefix1 - double unary minus cancels out"
      (is (= true (:ok s1)))
      (is (== 4 (:value s1))))))

(deftest test-prefix-default
  (let [s1 (parse (prefix factor uni-op 0.0) "")]
    (testing "prefix - returns default value when input is empty"
      (is (= true (:ok s1)))
      (is (== 0 (:value s1))))))

;; +-------------------------------------------------------------+
;; |                         postfix1 / postfix                  |
;; +-------------------------------------------------------------+

(deftest test-postfix1-no-ops
  (let [double-op (parse float-lit "42")
        s1        (parse (postfix1 float-lit add-op) "42")]
    (testing "postfix1 - single operand with no postfix operator"
      (is (= true (:ok s1)))
      (is (== 42 (:value s1))))))

;; +-------------------------------------------------------------+
;; |                    relational / boolean                     |
;; +-------------------------------------------------------------+

(deftest test-rel-op-greater-than
  (let [s1 (parse expr-p "5 > 3")]
    (testing "rel-op - greater than"
      (is (= true (:ok s1)))
      (is (= true (:value s1))))))

(deftest test-rel-op-equal
  (let [s1 (parse expr-p "2 + 2 == 4")]
    (testing "rel-op - arithmetic evaluated before relational"
      (is (= true (:ok s1)))
      (is (= true (:value s1))))))

(deftest test-rel-op-not-equal
  (let [s1 (parse expr-p "1 != 2")]
    (testing "rel-op - not equal"
      (is (= true (:ok s1)))
      (is (= true (:value s1))))))

;; +-------------------------------------------------------------+
;; |                         AST variants                        |
;; +-------------------------------------------------------------+

(declare ast-expr-p)
(def ast-factor  (<|> float-lit (parens (fwd ast-expr-p))))
(def ast-term    (chainl1* :BINOP ast-factor mul-op))
(def ast-expr-p  (chainl1* :BINOP ast-term   add-op))

(deftest test-chainl1*-produces-ast
  (let [s1 (parse ast-expr-p "1 + 2")]
    (testing "chainl1* - produces an AST node map"
      (is (= true (:ok s1)))
      (is (= :BINOP (:token (:value s1))))
      (is (= + (:op (:value s1))))
      (is (== 1 (:left (:value s1))))
      (is (== 2 (:right (:value s1)))))))

(deftest test-chainr1*-produces-ast
  (let [s1 (parse (chainr1* :BINOP ast-factor pow-op) "2 ^ 8")]
    (testing "chainr1* - produces an AST node map"
      (is (= true (:ok s1)))
      (is (= :BINOP (:token (:value s1))))
      (is (== 2 (:left (:value s1))))
      (is (== 8 (:right (:value s1)))))))
