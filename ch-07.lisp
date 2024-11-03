;;;; paip/ch-07.lisp
                   
;;;; Chapter 7: STUDENT: Solving Algebra
;;;; Word Problems
                 
(in-package :student)

#| STUDENT was written as a doctoral thesis
in the mid 1960s. At the time, it was 
considered state of the art at the time.
It was capable of reading a high-school
algebra word problem in plain English, 
and providing a numerically correct result.
The following is a nearly complete 
implementation of STUDENT. |#
               
(defstruct (rule (:type list))
  pattern
  response)

(defstruct (exp 
             (:type list)
             (:constructor 
                mkexp (lhs op rhs)))
  op
  lhs
  rhs)

(defun expp (x)
  (consp x))
  
(defun exp-args (x)
  (rest x))

(pat-match-abbrev '?x* '(?* ?x))

(pat-match-abbrev '?y* '(?* ?y))

(defparameter *student-rules*
  (mapcar
    #'expand-pat-match-abbrev
    '(((?x* |.|) ?x)
      ((?x* |.| ?y*) (?x ?y))
      ((if ?x* |,| then ?y*) (?x ?y))
      ((if ?x* then ?y*) (?x ?y))
      ((if ?x* |,| ?y*) (?x ?y))
      ((?x* |,| and ?y*) (?x ?y))
      ((find ?x* and ?y*)
       ((= to-find-1 ?x) (= to-find-2 ?y)))
      ((find ?x*) (= to-find ?x))
      ((?x* equals ?y*) (= ?x ?y))
      ((?x* same as ?y*) (= x? y?))
      ((?x* = ?y*) (= ?x ?y))
      ((?x* is equal to ?y*) (= ?x ?y))
      ((?x* is ?y*) (= ?x ?y))
      ((?x* - ?y*) (- ?x ?y))
      ((?x* minus ?y*) (- ?x ?y))
      ((difference between ?x* and ?y*)
       (- ?x ?y))
      ((difference ?x* and ?y*) (- ?x ?y))
      ((?x* + ?y*) (+ ?x ?y))
      ((?x* plus ?y*) (+ ?x ?y))
      ((sum ?x* and ?y*) (+ ?x ?y))
      ((product ?x* and ?y*) (* ?x ?y))
      ((?x* * ?y*) (* ?x ?y))
      ((?x* times ?y*) (* ?x ?y))
      ((?x* / ?y*) (/ ?x ?y))
      ((?x* per ?y*) (/ ?x ?y))
      ((?x* divided by ?y*)
       (/ ?x ?y))
      ((half ?x*) (/ ?x 2))
      ((one half ?x*) (/ ?x 2))
      ((twice ?x*) (* ?x 2))
      ((square ?x*) (* ?x ?x))
      ((?x* % less than ?y*)
       (* ?y (/ (- 100 ?x) 100)))
      ((?x* % more than ?y*)
       (* ?y (/ (+ 100 ?x) 100)))
      ((?x* % ?y*) (* (/ ?x 100) ?y)))))
      
(defun student (words)
  "Solve certain algebra word problems."
  (solve-equations
   (create-list-of-equations
    (translate-to-expression
     (remove-if #'noise-word-p words)))))

(defun translate-to-expression (words)
  "Translate an English phrase into an
  equation or expresssion."
  (or
   (rule-based-translator
    words
    *student-rules*
    :rule-if #'rule-pattern
    :rule-then #'rule-response
    :action #'(lambda (bindings response)
                (sublis
                 (mapcar 
                   #'translate-pair
                   bindings)
                 response)))
   (make-variable words)))

(defun translate-pair (pair)
  "Translate the value part of the pair
  into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression
         (binding-val pair))))

(defun create-list-of-equations (exp)
  "Separate out equations embedded in
  nested parentheses."
  (cond
   ((null exp)
    nil)
   ((atom (first exp))
    (list exp))
   (t
    (append
     (create-list-of-equations (first exp))
     (create-list-of-equations (rest exp))))))
          
(defun make-variable (words)
  "Create a variable name based on the given
  list of words."
  ;; The list of words will already have 
  ;; noise words removed.
  (first words))

 (defun noise-word-p (word)
   "Is this a low-content word that can 
   be safely ignored?"
   (member word
     '(a an the this number of $)))
 
(defun solve-equations (equations)
  "Print the equations and their solution."
  (print-equations 
    "The equations to be solved are:"
    equations)
  (print-equations
   "The solution is:"
   (solve equations nil)))
   
(defun solve (equations known)
  "Solve a system of equations by 
  constraint propagation."
  ;; Try to solve for one equation
  ;; and substitute its value into
  ;; the othets. If that doesn't work,
  ;; return what is known."
  (or
   (some
    #'(lambda (equation)
        (let ((x (one-unknown equation)))
          (when x
           (let ((answer 
                   (solve-arithmetic
                    (isolate equation x))))
             (solve
               (subst
                 (exp-rhs answer)
                 (exp-lhs answer)
                 (remove equation equations))
               (cons answer known))))))
    equations)
  known))    
  
(defun isolate (e x)
  "Isolate the lone x in e on the left-hand
  side of e."
  ;; This assumes that there is only one
  ;; x in e, and e is an equation.
  (cond
   ((eq (exp-lhs e) x)
    ;; Case 1: X = A -> X = n
    e)
   ((in-exp x (exp-rhs e))
    ;; Case 2: A = f(X) -> f(X) = A
    (isolate 
      (mkexp 
        (exp-rhs e) '= (exp-lhs e))
     x))
   ((in-exp x (exp-lhs (exp-lhs e)))
    ;; Case 3: f(X)*A = B -> f(X) = B/A
    (isolate
     (mkexp
       (exp-lhs (exp-lhs e))
       '=
       (mkexp (exp-rhs e)
         (inverse-op
           (exp-op
            (exp-lhs e)))
         (exp-rhs (exp-lhs e))))
     x))
   ((commutativep (exp-op (exp-lhs e)))
    ;; Case 4: A*f(X) = B -> f(X) = B/A
    (isolate
     (mkexp
      (exp-rhs
        (exp-lhs e))
      '=
      (mkexp
       (exp-rhs e)
       (inverse-op
        (exp-op
         (exp-lhs e)))
       (exp-lhs (exp-lhs e))))
     x))
   (t
    ;; Case 5: A/f(X) = B -> f(X) = A/B
    (isolate
     (mkexp 
       (exp-rhs
        (exp-lhs e))
       '=
       (mkexp
         (exp-lhs
           (exp-lhs e))
         (exp-op
           (exp-lhs e))
         (exp-rhs e)))
     x))))

(defun print-equations (header equations)
  "Print a list of equations."
  (format 
    t 
    "~%~A~{~% ~{ ~A~}~}~%"
    header
    (mapcar #'prefix->infix equations)))

(defconstant +operators-and-inverses+
  '((+ -) (- +) (* /) (/ *) (= =)))

(defun inverse-op (op)
  (second 
    (assoc op +operators-and-inverses+)))
    
(defun unknownp (exp)
  (symbolp exp))

(defun in-exp (x exp)
  "True if x appears anywhere in expression."
  (or (eq x exp)
      (and (expp exp)
           (or (in-exp x (exp-lhs exp))
               (in-exp x (exp-rhs exp))))))

(defun no-unknown (exp)
  "Returns true if there are no unknowns
  in exp."
  (cond
   ((unknownp exp)
    nil)
   ((atom exp)
    t)
   ((no-unknown (exp-lhs exp))
    (no-unknown (exp-rhs exp)))
   (t nil)))

(defun one-unknown (exp)
  "Returns the single unknown in an 
  expression, if there is only one."
  (cond
   ((unknownp exp)
    exp)
   ((atom exp)
    nil)
   ((no-unknown (exp-lhs exp))
    (one-unknown (exp-rhs exp)))
   ((no-unknown (exp-rhs exp))
    (one-unknown (exp-lhs exp)))
   (t nil)))

(defun commutativep (op)
  "Is operator commutative?"
  (member op '(+ * =)))

(defun solve-arithmetic (equation)
  "Do the arithmetic for the right-hand
  side."
  ;; This assumes that the right-hand side
  ;; is in the right form
  (mkexp 
    (exp-lhs equation) 
    '= 
    (eval
     (exp-rhs equation))))

(defun binary-exp-p (x)
  (and (expp x)
       (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions"
  (if (atom exp)
      exp
      (mapcar 
        #'prefix->infix
        (if (binary-exp-p exp)
            (list
             (exp-lhs exp)
             (exp-op exp)
             (exp-rhs exp))
            exp))))

(defun trace-student ()
  (trace rule-pattern
         rule-response
         exp-op
         exp-lhs
         exp-rhs
         expp
         exp-args
         student
         translate-to-expression
         rule-based-translator 
         translate-pair
         create-list-of-equations
         make-variable
         noise-word-p
         solve-equations
         solve
         isolate
         print-equations
         inverse-op
         unknownp
         in-exp
         no-unknown
         one-unknown
         commutativep
         solve-arithmetic
         binary-exp-p
         prefix->infix
         mkexp
         binding-var
         binding-val
         pat-match
         variablep
         match-variable
         get-binding
         make-binding
         extend-bindings))

#|

Exercise 7.1 [m] Implement print-equations 
using only primitive printing functions such 
as terpri and princ, along with explicit 
loops. |#
       
#|

(defun print-equations (header equations)
  "Print a list of equations."
  (terpri)
  (princ header)
  (dolist (e equations)
    (terpri)
    (princ (prefix->suffix e)))
  (terpri))

|#
         
#|
 
(student '(if the number of customers tom
           gets is twice the square of 
           20 % of the advertisements he
           runs |,| and the number of 
           advertisements is 45 |,| what
           is the number of customers
           tom gets ?)  
[printout] 
          
The equations to be solved are:
    CUSTOMERS = ((((20 / 100) *
                   ADVERTISEMENTS) *
                  ((20 / 100) *
                   ADVERTISEMENTS)) *
                2)
    ADVERTISEMENTS = 45
    WHAT = CUSTOMERS
The solution is:
    WHAT = 162
    CUSTOMERS = 162
    ADVERTISEMENTS = 45. 
-> NIL 

This correct. However, there are snags.
(1). The program does not always grab
the right variables.
(2). The program does not know how to 
solve simultaneous equations. 
         
The second is demonstrated by the following 
call:
         
(STUDENT
 '(THE DAILY
       COST
       OF
       LIVING
       FOR
       A
       GROUP
       IS
       THE
       OVERHEAD
       COST
       PLUS
       THE
       RUNNING
       COST
       FOR
       EACH
       PERSON
       TIMES
       THE
       NUMBER
       OF
       PEOPLE
       IN
       THE
       GROUP
       |.|
       THE
       COST
       FOR
       ONE
       GROUP
       EQUALS
       $
       100
       |,|
       AND
       THE
       NUMBER
       OF
       PEOPLE
       IN
       THE
       GROUP
       IS
       40
       |.|
       IF
       THE
       OVERHEAD
       COST
       IS
       10
       TIMES
       THE
       RUNNING
       COST
       |,|
       FIND
       THE
       OVERHEAD
       AND
       RUNNING
       COST
       FOR
       EACH
       PERSON
       |.|))
         
[Printout]
         

The equations to be solved are:
  DAILY = (OVERHEAD + (RUNNING * PEOPLE))
  COST = 100
  PEOPLE = 40
  OVERHEAD = (10 * RUNNING)
  TO-FIND-1 = OVERHEAD
  TO-FIND-2 = RUNNING

The solution is:
  PEOPLE = 40
  COST = 100    
         
The program prints the knowns as the 
results, with no attempt made to solve
the simultaneous equation. The equation
is not the right one.
         
3. Division by zero is neither recognized
nor caught. This can result in completely
incorrect answers, or a crash if the
division makes it into eval. |#
         

   
            
              

   
  
  
  


  
       
      
       
               

               
