;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.kern.test-helpers
  (:require #?(:clj  [clojure.test :refer [is]]
               :cljs [cljs.test    :refer [is]])))

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

(def space-string #?(:clj "\\space" :cljs " "))
