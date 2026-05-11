(defproject org.blancas/kern "1.2.0"
  :description "A Parser Combinators Library"
  :license {:name "Eclipse Public License"
	    :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/blancas/kern"
  :dependencies [[org.clojure/clojure "1.12.4"]
                 [org.clojure/clojurescript "1.12.134"]]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :jvm-opts ["-Dfile.encoding=UTF-8"]
  :deploy-repositories [["releases" :clojars]]
  :profiles
    {:dev {:resource-paths ["src/main/resources" "src/test/resources"]
           :dependencies [[org.clojure/tools.trace "0.8.0"]
                          [nrepl "1.3.0"]
                          [criterium "0.4.6"]]}})
