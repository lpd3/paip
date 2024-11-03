;;;; paip/ch-02.lisp
                   
;;;; Chapter 2: A Simple Lisp Program

;;; Task: construct a generative, context-free
;;; English grammar.

(in-package :02-simple-program)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase% () ; 1
  (append (article) (noun)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun article ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table)))

(defun verb ()
  (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a 
  list of it."
  (list (random-elt set)))
  
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
  
(defun adj ()
  (one-of '(big little blue green adiabiatic)))

(defun prep ()
  (one-of '(to in by on with)))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun pp* ()
  "Kleene star notation. Optional construct
  in a sentence."
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      nil))

(defun adj* ()
  (if (zerop (random 2))
      nil
      (append (adj) (adj*))))

(defun noun-phrase () ;; 2
  "Updated to include adj* and pp*"
  (append (article) (adj*) (noun) (pp*)))
  
;; The approach above works well with a 
;; small subset of English grammar. But it
;; relies heavily on its implementation.
;; We have hard-wired generation to specific
;; Lisp operators. Already, the appearance
;; of the code is ceasing to resemble the 
;; problem. As our grammar becomes more 
;; complex, this will only get worse. We 
;; need a different solution--one that places
;; the problem top and foremost, and allows
;; for a variety of implementations

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (noun -> man ball woman table)
    (verb -> hit took saw liked))
   "Trivial grammar for a subset of English.")

(defparameter *grammar* *simple-grammar*
  "The grammar currently in use.")

(defun rule-lhs (rule)
  "Retrieves the left-hand side of a rule,
  i.e., the grammatical category or key."
  (first rule))

(defun rule-rhs (rule)
  "Retrieves the right-hand side of a rule,
  i.e. the rule itself, or value."
  (rest (rest rule))) ; skip over ->
                                   
(defun rewrites (category)
  "Returns a list of the possible rewrites
  for this category."
  (rule-rhs (assoc category *grammar*)))
  
(defun generate% (phrase) ; 1
  "Generate a random sentence or phrase."
  (cond
   ((listp phrase) 
    (mappend #'generate phrase))
   ((rewrites phrase)
    (generate (random-elt (rewrites phrase))))
   (t (list phrase))))

(defun generate (phrase) ; 2
  "Generate a random sentence or phrase.
  Tiny improvement: calls rewrites only
  once."
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if choices
            (generate (random-elt choices))
            (list phrase)))))
            
#|

Exercise 2.1 [m] Write a version of generate 
that uses cond but avoids calling rewrites 
twice.

(defun generate (phrase)
  (cond
   ((listp phrase)
    (mappend #'generate phrase))
   (t
    (let ((choices (rewrites phrase)))
      (if choices
          (generate (random-elt choices))
          (list phrase))))))

Exercise 2.2 [m] Write a version of generate 
that explicitly differentiates between 
terminal symbols (those with no rewrite 
rules) and nonterminal symbols.
                              
(defun terminalp (subphrase)
  (not (rewrites subphrase)))
       
(defun generate (phrase)
  (cond
   ((listp phrase)
    (mappend #'generate phrase))
   ((terminalp phrase)
    (list phrase))
   (t
    (generate (random-elt (rewrites phrase))))))
|#
 
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article adj* noun pp*)
                    (name) (pronoun))
    (verb-phrase -> (verb noun-phrase pp*))
    (article -> the a)
    (adj* -> nil (adj adj*))
    (noun -> man ball woman table)
    (pp* -> nil (pp pp*))
    (name -> pat kim lee terry robin)
    (pronoun -> he she it those these that)
    (verb -> hit took saw liked)
    (adj -> big little blue green adiabiatic)
    (pp -> (prep noun-phrase))
    (prep -> to in by with on)))

(defun combine-all% (x-list y-list) ; 1
  "Return a list of lists formed by 
  appending a y to an x.
  E.g. (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))"
  (mappend
  #'(lambda (y)
      (mapcar 
      #'(lambda (x)
          (append x y))
        x-list))
    y-list))  

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond
   ((listp phrase)
    (mapcar #'generate-tree phrase))
   ((rewrites phrase)
    (cons phrase 
          (generate-tree
           (random-elt
            (rewrites phrase)))))
   (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible 
  expansions of this phrase."
  (cond
   ((null phrase)
    (list nil))
   ((listp phrase)
    (combine-all (generate-all (first phrase))
                 (generate-all (rest phrase))))
   ((rewrites phrase)
      (mappend #'generate-all 
               (rewrites phrase)))
   (t (list (list phrase)))))

#|
Exercise 2.3 [h] Write a trivial grammar for 
some other language. This can be a natural 
language other than English, or perhaps a 
subset of a computer language.
|#

(defparameter *gramática-sencilla-española*
  '((oración -> (frase-sustantiva frase-verbal))
    (frase-sustantiva -> (artículo sustantivo))
    (frase-verbal -> (verbo frase-sustantiva))
    (artículo -> la una)
    (sustantivo -> mujer mesa pelota secadora)
    (verbo -> golpó utilizó hizo vió)))

#|

Exercise 2.4 [m] One way of describing 
combine-all is that it calculates the 
cross-product of the function append on the 
argument lists. Write the higher-order 
function cross-product, and define 
combine-all in terms of it. |#    
            
;; Assuming there are only two lists,
;; and they are structured like the 
;; lists given to combine-all.

(defun cross-product (fn xlist ylist)
  (mappend
   #'(lambda (y)
       (mapcar 
        #'(lambda (x)
            (funcall fn x y))
          xlist))
     ylist))

(defun combine-all (xlist ylist)
  "Rewritten in terms of cross-product." ; 2
  (cross-product #'append xlist ylist))
            
    
    
              
   
    
   
    
  
    


         