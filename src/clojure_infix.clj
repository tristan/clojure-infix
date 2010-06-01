(ns clojure-infix
  (:require [clojure-infix.parser :as parser]))

(defn read-infix-string [s]
  (let [[f v] (parser/parse s)]
    (with-meta f {:variables v})))

(defmacro load-infix-string [s]
  `(eval (read-infix-string ~s)))