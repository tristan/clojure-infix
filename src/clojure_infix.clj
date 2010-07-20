(ns clojure-infix
  (:require [clojure-infix.parser :as parser]))

(defn read-infix-string [s]
  (let [[f v] (parser/parse s)]
    (if (instance? clojure.lang.IObj f)
      (with-meta f {:variables v})
      f)))

(defmacro load-infix-string [s]
  `(eval (read-infix-string ~s)))