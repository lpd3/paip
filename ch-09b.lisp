;;;; paip/ch-09b.lisp

;;;; Continuation of efficiency 
;;;; experiments with simplify.
;;;; This file investigates 
;;;; compilation of rules into
;;;; functions.
              
(in-package :compile-simplify)

(defun expp (x)
  (consp x))

(defun make-exp (lhs op &optional rhs)
  (if rhs
      (list op lhs rhs)
      (list op lhs)))

(defun exp-lhs (exp)
  (second exp))

(defun exp-rhs (exp)
  (third exp))

(defun exp-op (exp)
  (first exp))

(defun variablep (x)
  (member x
    '(x y z m n o p q r s t u v w)))

(defvar *bindings* nil
  "A list of bindings used by the
  rule compiler.")

(defun compile-rule (rule)
  "Compile a single rule."
  (let ((*bindings* nil))
    `#'(lambda (x)
         ,(compile-exp 
            'x
            (exp-lhs rule) 
            ;; 'x is the lambda parameter
            (delay
             (build-exp 
               (exp-rhs rule)
               *bindings*))))))

(defun compile-exp (var pattern consequent)
  "Compile code that tests the expression,
  and performs the consequent if the
  expression matches the pattern.
  Assumes that *bindings* contains
  the appropriate bindings."
  (cond
   ((get-binding pattern *bindings*)
    ;; Test a previously bound variable
    `(when 
       (equal 
         ,var 
         ,(lookup pattern *bindings*))
       ,(force consequent)))
   ((variablep pattern)
    ;; Add new bindings. Do type checking
    ;; if needed.
    (push (cons pattern var) *bindings*)
    (force consequent))
   ((atom pattern)
    ;; match an atom literal
    `(when (eql ,var ',pattern)
       ,(force consequent)))
   ((starts-with pattern '?is)
    (push (cons (second pattern) var)
          *bindings*)
    `(when ( ,(third pattern) ,var)
       ,(force consequent)))
   ;; So far, only the ?is pattern is 
   ;; covered, because it is the only 
   ;; one used in the simplification
   ;; rules. Other patterns could be 
   ;; compiled by adding code here. 
   ;; Or, we could switch to a data-
   ;; driven approach.
   (t 
     ;; check the operator and arguments.
     `(when (op? ,var ',(exp-op pattern))
        ,(compile-args
           var 
           pattern
           consequent)))))

(defun compile-args (var pattern consequent)
  "Compile code that checks the arg or 
  args, and performs the consequent if 
  the args match."
  ;; First, make up variable names for 
  ;; the args.
  (let ((l (concat-symbol var 'l))
        (r (concat-symbol var 'r)))
    (if (exp-rhs pattern)
        ;; two-arg case
        `(let ((,l (exp-lhs ,var))
               (,r (exp-rhs ,var)))
           ,(compile-exp 
              l 
              (exp-lhs pattern)
              (delay
               (compile-exp
                r
                (exp-rhs pattern)
                consequent))))
         ;; one-arg case
         `(let ((,l (exp-lhs ,var)))
            ,(compile-exp
              l
              (exp-lhs pattern)
              consequent)))))

(defun build-exp (exp bindings)
  "Compile code that will build the exp,
  given the bindings."
  (cond
   ((assoc exp bindings)
    (rest (assoc exp bindings)))
   ((variablep exp)
    (error 
      "Variable ~A occurred on ~%~
      right hand side but not left~%~
      BUILD-EXP."
      exp))
   ((atom exp)
    `',exp)
   (t
    (let ((new-exp
           (mapcar
            #'(lambda (x)
                (build-exp x bindings))
              exp)))
      `(simplify-exp (list ,@new-exp))))))

(defun op? (exp op)
  "Does the expression have the given op
  as its operator?"
  (and
   (expp exp)
   (eq (exp-op exp) op)))

(defun concat-symbol (&rest args)
  "Concatenate symbols or strings to
  form an interned symbol."
  (intern 
    (format nil "~{~A~}" args)))

;; the following is a general utility, 
;; not used here.

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to
  form an uninterned symbol."
  (make-symbol
   (format nil "~{~A~}" args)))

(clear-props 'n 'm 's)

(pat-match-abbrev 'n '(?is n numberp))

(pat-match-abbrev 'm '(?is m numberp))

(pat-match-abbrev 's '(?is s not-number-p))
  
;; Now some code to compile a collection
;; of rules

(defun compile-rule-set (op)
  "Compile all rules indexed under a given
  main op, and make them into the simp-fn
  for that op."
  (set-simp-fn
   op
   (compile
     nil
     `(lambda (x)
        ,(reduce 
           #'combine-rules
           (mapcar 
             #'compile-indexed-rule
             (rules-for op)))))))

(defun compile-indexed-rule (rule)
  "Compile one rule into lambda-less
  code, assuming indexing of main op."
  (let ((*bindings* nil))
    (compile-args
     'x
     (exp-lhs rule)
     (delay 
       (build-exp
        (exp-rhs rule)
        *bindings*)))))

(defun combine-rules (a b)
  "Combine the code for two rules into
  one, maintaining order."
  ;; In the default case, we generate the
  ;; code (or a b),
  ;; but we try to be cleverer and share 
  ;; common code, on the assumption that
  ;; there are no side-effects.
  (cond
   ((and 
      (listp a) 
      (listp b)
      (= (length a) (length b) 3)
      (equal (first a) (first b))
      (equal (second a) (second b)))
    ;; a = (f x y), b = (f x z) =>
    ;; (f x (combine-rules y z))
    ;; can only apply if f=let or f=if
    (list
     (first a)
     (second a)
     (combine-rules (third a) (third b))))
   ((matching-ifs a b)
    `(if ,(second a)
         ,(combine-rules
           (third a)
           (third b))
         ,(combine-rules
           (fourth a)
           (fourth b))))
   ((starts-with a 'or)
    ;; a = (or ... (if p y)),
    ;; b = (if p z) =>
    ;; (or ... (if p (combine-rules y z)))
    (if (matching-ifs (last1 a) b)
        (append
         (butlast a)
         (list
          (combine-rules
           (last1 a)
           b)))
        (append a (list b))))
   (t
    ;; a b => (or a b)
    `(or ,a ,b))))

(defun matching-ifs (a b)
  "Are a and b IF statements with the 
  same predicate?"
  (when (consp a)
    (when (member (first a) '(if when))
      (when (consp b)
        (when (member (first b) '(if when))
          (equal (second a) (second b)))))))
    

(defun last1 (list)
  "Return the last element of a list"
  (first (last list)))

(defun compile-all-rules-indexed (rules)
  "Compile a separate fn for each operator,
  and store it as the simp-fn of the 
  operator."
  (index-rules rules)
  (let ((all-ops
         (delete-duplicates
          (mapcar #'main-op rules))))
    (mapc #'compile-rule-set all-ops)))
    
;; Let's compile those rules

(compile-all-rules-indexed
  *simplification-rules*)

;; Finally, we are ready for a new 
;; simplify-exp function

(defun simplify-exp (exp)
  "Simplify by doing arithmetic, or 
  by using the simp-fn for this 
  operator. Do not use rules of 
  any kind."
  (cond
   ((simplify-by-fn exp))
   ((evaluable exp) (eval exp))
   (t exp)))
   
;; Now, we need the simplify fn again

(defun simplify (exp)
  "Simplify an expression by first 
  simplifying its components."
  (if (atom exp)
      exp
      (simplify-exp
        (mapcar #'simplify exp))))

;; And the profiling apparatus

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
                *test-data*))
             (mapcar #'simplify
               *test-data*))))
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

#| 
  
Unfortunately, this didn't work. It does
not simplify derivatives because the
final x of the expression is subject
to variable capture from the variables 
created by the symbol function.
        
|#
    







        
     
    
     
    
      
         
  

