(ns clojure-infix.core-test
  (:use [clojure-infix] :reload-all)
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write ; TODO: more tests!!!!!
  (= 3 (load-infix-string "max(2,3)")))
