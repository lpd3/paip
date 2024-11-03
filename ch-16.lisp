;;;; paip/ch-16.lisp

;;;; Chapter 16: Expert Systems
                              
(in-package :expert)

#| Expert systems are programming abstractions
designed to capture the knowledge base of 
an expert, in the hopes of solving 
problems.

These are rule-based languages, like Prolog,
but have these important features lacking 
in Prolog:
   
1. They reason with probabilities rather
than true-false.
     
2. They can provide explanations for the 
logic behind decisions
      
3. They offer flexible and modifiable 
control structure.
        
Among the best known of these was Mycin, 
which solved problems in the domain 
of infectious disease diagnosis.
   
Emycin was a successor that lacked a 
knowledge base, but could acquire one.
          
|#

;; Since our logic is not Boolean, we 
;; must design new logical constructs

(defconstant true +1.0)

(defconstant false -1.0)

(defconstant unknown 0.0)

;; The Emycin 'combine' function used the
;; following function to combine results:

;; combine(A, B) = A + B - AB; A, B > 0
;;               = A + B + AB; A, B < 0
;;               = (A + B)/(1 - min(A, B)); 
;; otherwise

;; Emycin operates differently when 
;; combining more than one probability.
;; It assumes that two probabilities are
;; independent, but more than two are 
;; dependent. In the latter case, it uses
;; the minimum of each conjunct's certainty
;; factor.

(defun cf-or (a b)
  "Combine the certainty factors for the 
  formula (A or B). This is used when two
  rules support the same conclusion."
  (cond
   ((and (> a 0) (> b 0))
    (+ a b (* -1 a b)))
   ((and (< a 0) (< b 0))
    (+ a b (* a b)))
   (t
    (/ (+ a b)
       (- 1 (min (abs a) (abs b)))))))

(defun cf-and (a b)
  "Combine the certainty factors for 
  the formula (A and B)."
  (min a b))

;; Whereas Prolog stops searching when any
;; proposition is false, Emycin stops when
;; the probability is below a certain 
;; threshold.

(defconstant cf-cut-off 0.2
  "Below this certainty, we cut off search.")

(defun true-p (cf)
  "Is this certainty factor considered 
  true?"
  (and (cf-p cf)
       (> cf cf-cut-off)))

(defun false-p (cf)
  "Is this certainty factor considered 
  false?"
  (and (cf-p cf)
       (< cf (- cf-cut-off 1.0))))
       
(defun cf-p (x)
  "Is x a valid numeric certainty factor?"
  (and (numberp x) (<= false x true)))
  
#| Exercise 16.1
Suppose you saw a tabloid newspaper with
the headline "Elvis alive in Kalamazoo", 
and this statement has a probability of
0.01. How many more copies of the paper
would you need to see to reach 0.95 
certainty?

I ran a program. 298 copies.
|#

#| Caching Derived Facts |#
                          
(let ((db (make-hash-table :test #'equal)))
  (defun get-db (key) (gethash key db))
  (defun put-db (key val)
    (setf (gethash key db) val))
  (defun clear-db () (clrhash db)))

(defun get-vals (parm inst)
  "Return a list of (val cf) pairs 
  for this (parm inst)."
  (get-db (list parm inst)))

(defun get-cf (parm inst val)
  "Look up the certainty factor, or 
  return +unknown+."
  (or (second (assoc val (get-vals parm inst)))
      unknown))

(defun update-cf (parm inst val cf)
  "Change the certainty factor for 
  (parm inst is val), by combining the
  given cf with the old."
  (let 
      ((new-cf (cf-or 
                 cf 
                 (get-cf parm inst val))))
    (put-db (list parm inst)
            (cons
             (list val new-cf)
             (remove 
               val 
               (get-db (list parm inst))
               :key #'first)))))

#| Asking Questions
   
Emycin can automatically ask questions
of the user if the solution to the problem
can not be found by means of the rule-base
 alone.
 
|#

(defconstant help-string
  "~&Type one of the following:
 ?     - to see possible answers for this parameter
 rule  - to show the current rule
 why   - to see why this question is asked
 help  - to see this list
 xxx   - (for some specific xxx) if there is a definite answer
 (xxx .5 yyy .4) - if there are several answers with
                   different certainty factors.")
                                                                                              
(defun ask-vals (parm inst)
  "Ask the user for the value(s) of inst's 
  parm parameter, unless this has already
  been asked. Keep asking until the user
  types UNKNOWN (return nil) or a valid 
  reply (return t)."
  (unless (g2et-db `(asked ,parm ,inst) t)
    (loop
      (let ((ans 
              (prompt-and-read-vals 
                parm 
                inst)))
        (case ans
          (help (format help-string))
          (why 
            (print-why 
              (get-db 'current-rule) parm))
          (rule 
            (princ (get-db 'current-rule)))
          ((unk unknown) (return nil))
          (? 
            (format 
              t 
              "~&A ~a must be of type ~a"
              parm
              (parm-type parm))
            nil)
          (t 
            (if 
              (check-reply ans parm inst)
              (return t)
              (format 
                t 
                "~&Illegal reply. ~
                Type ? to see legal ~
                ones."))))))))

(defun prompt-and-read-vals (parm inst)
  "Print the prompt for this parameter
  (or make one up) and read the reply."
  (fresh-line)
  (format 
    t 
    (parm-prompt (get-parm parm))
    (inst-name inst)
    parm)
  (princ " ")
  (finish-output)
  (funcall (parm-reader (get-parm parm))))

(defun inst-name (inst)
  "The name of this instance."
  ;; The stored name is either like
  ;; (("Jan Doe" 1.0)) or nil.
  (or (first (first (get-vals 'name inst)))
      inst))

(defun check-reply (reply parm inst)
  "If reply is valid for this parm, update
  the DB. Reply should be a val or 
  (val1 cf1 val2 cf2 ...). Each val must
  be of the right type for this parm."
  (let ((answers (parse-reply reply)))
    (when 
      (every 
      #'(lambda (pair)
          (and 
            (typep 
              (first pair) 
              (parm-type parm))
            (cf-p (second pair))))
        answers)
      ;; Add replies to the data base
      (dolist (pair answers)
        (update-cf 
          parm 
          inst 
          (first pair) 
          (second pair)))
      answers)))

(defun parse-reply (reply)
  "Convert the reply into a list of 
  (value cf) pairs."
  (cond ((null reply) nil)
        ((atom reply) `((,reply ,true)))
        (t 
          (cons 
            (list 
              (first reply) 
              (second reply))
            (parse-reply
#| rest2 |# (rest2 reply)))))) ;;;; rest2
          
(defstruct (parm 
      (:constructor
       new-parm 
         (name 
          &optional 
          context-type-restriction
          prompt
          ask-first
          reader)))
  name 
  (context nil)
  (prompt "~&What is the ~*~a of ~2:*~a?")
  (ask-first nil)
  (type-restriction t)
  (reader 'read))

(defmacro defparm (parm &rest args)
  "Define a parameter."
  `(setf (get ',parm 'parm)
         (apply #'new-parm ',parm ',args)))

(defun parm-type (parm-name)
  "What type is expected for a value of this
  parameter?"
  (parm-type-restriction 
    (get-parm parm-name)))
    
(defun get-parm (parm-name)
  "Look up the parameter structure with this
  name."
  ;; if there is none, make up one.
  (or (get parm-name 'parm)
      (setf 
        (get parm-name 'parm) 
        (new-parm parm-name))))
        
(deftype yes/no () '(member yes no))

#| 16.4 Context instead of variables.

Prolog has logic variables. Emycin has 
contexts, constraints within which the 
program reasons. Contexts will be implemented
as data types. There will be three context
types:
patients, cultures, organisms.
          
More than one instance of the same type of
context cannot be used in the same operation.
        
|#

(defstruct context
  "A context is a subdomain, a type."
  name
  (number 0)
  initial-data
  goals)

(defmacro defcontext (name 
                      &optional
                      initial-data
                      goals)
  "Define a context."
  `(make-context
    :name ,name
    :initial-data ,initial-data
    :goals ,goals))

(defun new-instance (context)
  "Creates a new instance of this context."
  (let ((instance (format nil "~a-~d"
                    (context-name context)
                    (incf 
                      (context-number 
                        context)))))
    (format t "~&------ ~a ------~&"
      instance)
    (put-db (context-name context) instance)
    (put-db 'current-instance instance)))
                          
#| Backward-chaining Revisited |#
                            
                   
        
    




                                                    

