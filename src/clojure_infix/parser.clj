(ns clojure-infix.parser
  (:require [clojure.contrib [str-utils :as str-utils]]))

(def *debug* false)

(defn operator-weights [op]
  (get {\) 0
	\> 1
	\] 2 ; >=
	\< 3
	\[ 4 ; <=
	\= 5
	\+ 6
	\- 7
	\* 8
	\/ 9
	\^ 10
	\( 100}
       op
       0))

(defn str-to-obj [string]
  (cond (not (string? string)) ; if it's not a string
	(throw (Exception. "why are you passing a non string to a str-to-obj function?!"))
	(re-find #"^[\+\-\*\/\^\%\(\)\<\>\=]$" string) ; if it's an operator
	(first string) ; convert it to a character
	(= string "==")
	\=
	(= string ">=")
	\]
	(= string "<=")
	\[
	(re-find #"^[\-]*[\d]+$" string) ; if it's a number
	(Integer/parseInt string)
	(re-find #"^[\-]*[\d]+[.][\d]+$" string) ; if it's a decimal number
	(Double/parseDouble string)
	(re-find #"^[a-zA-Z_]+[\w]*$" string) ; if it's a variable name
	(symbol string) ; convert to a symbol
	(re-find #"^[a-zA-Z_]+[\w]*\($" string) ; if it's a function call
	(fn [] (symbol (str "Math/" (re-find #"^[a-zA-Z_]+[\w]*" string)))) ; return a function which returns the function's symbol
	:else
	(throw (Exception. (str "Unable to find a suitible object for string: " string)))))

(defn tokenise [infix]
  (map #(str-to-obj %)
       (filter #(and (nil? (re-find #"^[\s]+$" %)) (not (= "" %))) 
	       (str-utils/re-partition #"[ \(\)\*\+\-\^\%\/]|\=\=|\>\=|\<\=|[\=\>\<]|[\w]+\(" infix))))

(defn char-to-symbol [char]
  ; convert operaters in chars to their equivalent symbols
  (get {\+ '+
	\> '>
	\< '<
	\] '>=
	\[ '<=
	\= '=
	\- '-
	\* '*
	\/ '/
	\^ 'Math/pow
	\% 'mod} char))

(defn oper-join 
  ([ops- vals-]
     (let [[_ v] (oper-join ops- vals- -1)]
       (if (= (count v) 1)
	 (first v)
	 (throw (java.text.ParseException. "found multiple numbers/variables in succession with no operator" 0)))))
  ([ops- vals- stop-at-op-weight]
  (if (or (empty? ops-) (<= (operator-weights (peek ops-)) stop-at-op-weight))
    [ops- vals-]
    (let [[o v] (let [op (peek ops-)]
		  (loop [ops (pop ops-) vals (pop (pop vals-)) r `(~(peek (pop vals-)) ~(peek vals-))]
		    (if (and (= (peek ops) op) (not (= op \^)))
		      (recur (pop ops) (pop vals) (cons (peek vals) r))
		      [ops (conj vals `(~(char-to-symbol op) ~@r))])))]
      (recur o v stop-at-op-weight)))))

(defn parse- [infix]
  (when *debug* (println "**********push**********"))
  (loop [tokens infix
	 value-stack []
	 operator-stack []
	 variables []
	 expecting-value? true] ; value here means number? symbol? fn? seq? or (
    (when *debug* 
      (println "----------------------------")
      (println tokens)
      (println value-stack)
      (println operator-stack)
      (println variables)
      (println expecting-value?))
    (if (empty? tokens)
      (do
	(when *debug* (println ".............pop............."))
      [nil
       (oper-join operator-stack value-stack)
       variables])
      (cond (and expecting-value? (or (number? (first tokens)) (symbol? (first tokens)) (seq? (first tokens))))
	    (recur (rest tokens)
		   (conj value-stack (first tokens))
		   operator-stack
		   (if (and (symbol? (first tokens)) (not (some #{(first tokens)} variables)))
		     (conj variables (first tokens)) variables)
		   false)
	    (and expecting-value? (or (fn? (first tokens)) (and (char? (first tokens)) (= (first tokens) \()))) ; fn or (
	    (let [[remaining-tokens val vars] (parse- (rest tokens))
		  vars (filter #(not (some #{%} variables)) vars)]
	      (recur remaining-tokens
		     (conj value-stack 
			   (if (fn? (first tokens))
			     `(~((first tokens)) ~val)
			     val))
		     operator-stack
		     (cond (empty? variables) (vec vars) ; ARGH! vec...
			   (empty? vars) variables
			   :else (apply conj (cons variables vars)))
		     false))
	  ; the remaining valid cases are "-" and "+"
	  (and expecting-value? (char? (first tokens)) (= (first tokens) \+)) ; ignore +'s, they're valid syntax but add nothing
	  (recur (rest tokens) value-stack operator-stack variables true)
	  (and expecting-value? (char? (first tokens)) (= (first tokens) \-)) ; - is a bit trickier
	  (cond (and (char? (second tokens)) (= (second tokens) \()) ; if the next token is an (
		; replace with a fn which returns the - symbol
		(recur (cons (fn [] '-) (rest (rest tokens))) value-stack operator-stack variables true)
		(number? (second tokens)) ; if the next token is a number
		(recur (cons (- (second tokens)) (rest (rest tokens))) value-stack operator-stack variables true)
		(symbol? (second tokens)) ; if the next token is a symbol
		(recur (rest (rest tokens)) ; add the negative of that symbol to the value stack
		       (conj value-stack `(~'- ~(second tokens)))
		       operator-stack
		       (if (some #{(second tokens)} variables)
			 variables
			 (conj variables (second tokens)))
		       false)
		(fn? (second tokens)) ; if we have a fn
		(let [[remaining-tokens val vars] (parse- (rest tokens))
		      vars (filter #(not (some #{%} variables)) vars)] ; process the function, then wrap it
		  (recur remaining-tokens
			 (conj value-stack
			       `(~'- ~val))
			 operator-stack
			 (cond (empty? variables) (vec vars)
			       (empty? vars) variables
			       :else (apply conj (cons variables vars)))
			 false))
		(and (char? (second tokens)) (= (second tokens) \+)) ; ignore +
		(recur (cons \- (rest (rest tokens))) value-stack operator-stack variables true)
		(and (char? (second tokens)) (= (second tokens) \-)) ; another - cancels this out
		(recur (rest (rest tokens)) value-stack operator-stack variables true)
		:else ; otherwise i don't know what to do!
		(throw (java.text.ParseException. (str "\"-\" before \"" (second tokens) "\" does not make sense") 0)))
	  (and expecting-value? (char? (first tokens)) (= (first tokens) \*) (= (peek operator-stack) \*)) ; handle pythonic ^ (**)
	  (recur (rest tokens) value-stack (conj (pop operator-stack) \^) variables true)
	  expecting-value? ; if we are expectinga value and it wasn't handled above we have problems!
	  (throw (java.text.ParseException. (str "Expecting value type, instead found: \"" (first tokens)
						 "\" <" (type (first tokens)) ">") 0))
	  (and (char? (first tokens)) (not (= \( (first tokens))))
	  (cond (= (first tokens) \)) ; if we have a close bracket
		(do
		  (when *debug* (println "..............pop............."))
		  [(rest tokens)
		   (oper-join operator-stack value-stack)
		   variables])
		; if this is the found operator we've encountered or an operator of equal or higher precidence
		(or (empty? operator-stack) (>= (operator-weights (first tokens)) (operator-weights (peek operator-stack))))
		(recur (rest tokens) value-stack (conj operator-stack (first tokens)) variables true)
		(and (= (peek operator-stack) \^) (= (first tokens) \*) (= (second tokens) \*)) ; edge case when using pythonic ^
		(recur (rest (rest tokens)) value-stack (conj operator-stack \^) variables true)
		; if we have a lower priority operator
		(< (operator-weights (first tokens)) (operator-weights (peek operator-stack)))
		(let [[o v] (oper-join operator-stack value-stack (operator-weights (first tokens)))]
		  (recur (rest tokens)
			 v
			 (conj o (first tokens))
			 variables
			 true))
		:else ; anything else, i don't yet know how to process
		(throw (java.text.ParseException. (str "Expecting operator or close bracket, instead found: \"" (first tokens)
						       "\" <" (type (first tokens)) ">") 0))
		)
	  :else ; did i forget anything?
	  (throw (java.text.ParseException. (str "Action for " (first tokens) "<" (type (first tokens)) "> not defined") 0))))))

(defn parse [infix]
  ; do some initial validation
  (when (odd? (count (filter #(or (= \( %) (= \) %)) infix))) 
    ; this doesn't catch people doing silly things like "2+3+4)+(5+6+7". However such cases are delt with post processing.
    (throw (java.text.ParseException. (str "Formula contains unmatched parentheses") 0)))
  ; tell people they're silly if they use both ^ and ** in the same equation
  (when (and (re-find #"\^" infix) (re-find #"\*\*" infix))
    (throw (java.text.ParseException. (str "the operators ^ and ** are both represent the \"power of\". "
					   "for claritys sake, please be consistent and use only one of these.") 0)))
  (let [[r f v] (parse- (tokenise infix))]
    (if (empty? r) ; if it's not empty, a close bracket has occured without a matching open bracket
      [f v]
      (throw (java.text.ParseException. (str "Formula contains unmatched parentheses") 0)))))

(defn parse-infix [infix]
  ; returns the list of variable names
  ; inside this infix function
  (let [[f v] (parse infix)]
    (when *debug*
      (println "&&&&&")
      (println infix)
      (println f (type f))
      (println v (type v))
      )
    [(eval `(fn ~v ~f)) v]))

(when *debug*
;  (println ((first (parse-infix "1.0+2.0-3.0/4.0*5.0^6.0*7.0/8.0-9.0+10.0"))))
;  (parse-infix "1.0+2.0*3.0-1.0")
  (time
  (println ((first
  (parse-infix "3.0+4.0*5.0*sin(a*(1.0+7.0/5.0*cos((b+1.0)*2.0)^2.0))/2.0+pi-(((9.0)))") ; = (a=5,b=6,pi=3.14) = -11.997034192773963
  ) 5.0 6.0 3.14)) ; if we pass integers in rather than floats clojure still returns this value, however due to 7/5 being
					; rounded down to 1 in pure java/python the result is wrong
  )
)

; (+ 3 (* 4 5 (/ (Math/sin (* a (+ 1 (* (/ 7 5) (Math/pow (Math/cos (* (+ b 1) 2)) 2))))) 2)) (- pi 9))
; (+ 3 (* 4 5 (/ (Math/sin (* a (+ 1 (* (/ 7 5) (Math/pow (Math/cos (* (+ b 1) 2)) 2))))) 2)) (- pi 9))