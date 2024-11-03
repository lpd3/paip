;;;; paip/ch-09a.lisp

;;;; Revisiting the simplify program
;;;; of chapter 8 employing the tools
;;;; of chapter 9.
                 
(in-package :efficient-simplify)

;;; The first thing is to copy
;;; the test suite from efficiency.

(defvar *test-data*
  (mapcar #'infix->prefix
    '((d (a * x ^ 2 + b * x + c) / d x)
      (d ((a * x ^ 2 + b * x + c) / x) / d x)
      (d ((a * x ^ 3 + b * x ^ 2 + 
           c * x + d) / x ^ 5) / d x)
      ((sin (x + x)) * (sin (2 * x)) +
            (cos (d (x ^ 2) / d x)) ^ 1)
      (d (3 * x + (cos x) / x) / d x))))

(defvar *answers*
  (mapcar #'symbolic:simplify *test-data*))

(defun test-it (&optional
                (with-profiling t))
  "Time a test run and make sure the 
  answers are correct."
  (let ((answers
         (if with-profiling
             (with-profiling
              '(simplify 
                simplify-exp
                variablep
                match-variable
                pat-match)
              (mapcar 
                #'simplify 
                *test-data*)))))
    (mapc #'assert-equal answers *answers*)
    t))

(defun assert-equal (x y)
  "If x is not equal to y, complain."
  (assert 
    (equal x y) 
    (x y)
    "Expected ~A to be equal to ~A"
    x
    y))
    
;;; When profiling was run on the 
;;; original version of symbolic, 
;;; it revealed that the lion's share
;;; of calls was to pat-match and 
;;; variablep. We can get significantly
;;; fewer calls by 1. indexing 
;;; simplification rules by operator, 
;;; so only the pertinent rules are 
;;; considered and 2. memoizing simplify,
;;; so that previously calculated results
;;; are not needlessly recalculated.
                                   
;;; First, indexing
                  
;;; We first repeat the definition of 
;;; simplify, so that it has access
;;; to the new simplify-exp.


(defun simplify (exp)
  "Simplify an expression by first 
  simplifying its components."
  (if (atom exp)
      exp
      (simplify-exp
        (mapcar #'simplify exp))))

(defun simplify-exp (exp)
  "Simplify using a rule, or by 
  doing arithmetic, or by using the 
  simp function supplied for this 
  operator. This version indexes 
  simplification rules under the 
  operator."
  (cond
   ((simplify-by-fn exp))
   ((rule-based-translator
     exp
     (rules-for (exp-op exp))
     :rule-if #'exp-lhs
     :rule-then #'exp-rhs
     :action
       #'(lambda (bindings response)
           (simplify 
             (sublis bindings response)))
     :var? #'symbolic:variablep))
   ((evaluable exp) (eval exp))
   (t exp)))

(defvar *rules-for*
  (make-hash-table :test #'equal)
  "Stores indexed patterns, keyed by
  function.")

(defun main-op (rule)
  "Retrieves the principal operation
  from a pattern."
  (exp-op (exp-lhs rule)))

(defun index-rules (rules)
  "Index all the rules under the main op."
  (clrhash *rules-for*)
  (dolist (rule rules)
    ;; nconc instead of push to preserve
    ;; the order
    (setf
     (gethash (main-op rule) *rules-for*)
     (nconc
      (gethash (main-op rule) *rules-for*)
      (list rule)))))

(defun rules-for (op) 
  "Given an operator, retrieves the 
  associated rule list, if any."
  (gethash op *rules-for*))

;; now we are ready to index the rules

(index-rules *simplification-rules*)

;; and finally, we can memoize the
;; simplify function.

(memoize 'simplify :test #'equal)

;; Now, because we have redefined 
;; simplify-exp, we should repeat
;; the definition for simplify here.
;; It has been shadowed.

(defun simplify (exp)
  "Simplify an expression by first 
  simplifying its components."
  (if (atom exp)
      exp
      (simplify-exp
        (mapcar #'simplify exp))))

#|

(test-it)
[print-out]
Total elapsed time: 0.106306d0 seconds.
Count Secs Time% Name
  12915   0.07 62% PAT-MATCH
  10654   0.01 13% VARIABLEP
   1317   0.02 16% MATCH-VARIABLE
    856   0.00 4%  SIMPLIFY
    258   0.01 6%  SIMPLIFY-EXP
-> T
   
Overall time decreased by 58%
Number of calls to pat-match is 1/4
       of previous
Number of calls to match-variable is
       between 1/4 and 1/3 of previous.
       
The new implementation was a success.
    
We can further speed things up by 
compiling the rules into functions. That
is the subject of the next file.
   
|#
    

   




    


