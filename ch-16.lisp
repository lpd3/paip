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
                          
#| Backward-chaining Revisited

Backward-chaining in Mycin is similar to backward-chaining
in Prolog. The main difference is the Mycin certainty
factors. These require that all relevant facts be gathered
before evaluating the goal.

Prolog can search depth-first. Mycin must search
breadth-first.

The functions below will be called in the following
order:

find-out: To find a parameter for an instance
  get-db: See if it is cached.
  ask-vals: Query the user
  use-rules: See if a known rule can supply the answer
    reject-premise: Try to disprove premise
    satisfy-premise: See if each condition is met
      eval-condition: Evaluate each condition
        find-out: By finding the parameter's values.
|#

(defstruct (rule (:print-function print-rule))
  number
  premises
  conclusion
  cf)

(let ((rules (make-hash-table)))
  (defun put-rule (rule)
    "Put the rule in a table, indexed under each
    rule in the table"
    (dolist (concl (rule-conclusions rule))
      (push rule (get-hash (first concl) rules)))
    rule)
  
  (defun get-rules (parm)
    "A list of rules that help determine this parameter."
    (gethash parm rules))

  (defun clear-rules () (clrhash rules)))

(defun find-out (parm &optional (inst (get-db 'current-instance)))
  "Find the value(s) of this parameter for this instance,
   unless the values are already known.
   Some parameters we ask first, others we use rules 
   first."
  (or (get-db `(known ,parm ,inst))
      (put-db `(known ,parm ,inst)
	      (if (parm-ask-first (get-parm parm))
		  (or (ask-vals parm inst) (use-rules parm))
		  (or (use-rules parm) (ask-vals parm inst))))))

(defun use-rules (parm)
  "Try every rule associated with this parameter.
  Return true if one of the rules returns true."
  (some #'true-p (mapcar #'use-rule (get-rules parm))))

(defun use-rule (rule)
  "Apply a rule to the current situation."
  ;; Keep track of the rule for the explanation system:
  (put-db 'current-rule rule)
  ;; If any premise is known false, give up.
  ;; If every premise can be proved true, then
  ;; draw conclusions (weighted by the certainty factor)
  (unless (some #'reject-premise (rule-premises rule))
    (let ((cf (satisfy-premises (rule-premises rule) true)))
      (when (true-p cf)
	(dolist (conclusion (rule-conclusions rule))
	  (conclude conclusion (* cf (rule-cf rule))))
	cf))))

(defun satisfy-premises (premises cf-so-far)
  "A list of premises is satisfied if they are all true.
  A combined cf is returned."
  ;; cf-so-far is an accumulator of certainty factors.
  (cond ((null premises) cf-so-far)
	((not (true-p cf-so-far)) false)
	(t (satisfy-premises
	    (rest premises)
	    (cf-and cf-so-far
		    (eval-condition (firsr premises)))))))

(defun eval-condition (condition &optional (find-out-p t))
  "See if this condition is true, optionally using FIND-OUT
  to determine unknown parameters."
  (multiple-value-bind (parm inst op val)
      (parse-condition condition)
    (when find-out-p
      (find-out parm inst))
    ;; Add up all the (val cf) pairs that satisfy the test
    (loop for pairs in (get-vals parm inst)
	  when (funcall op (first pair) val)
	    sum (second pair))))

(defun reject-premise (premise)
  "A premise is rejected if it is known false, without
  needing to call find-out recursively."
  (false-p (eval-condition premise nil)))

(defun conclude (conclusion cf)
  "Add conclusion (with specified cf) to DB."
  (multiple-value-bind (parm inst op val)
      (parse-condition conclusion)
    (update-cf parm inst val cf)))

(defun is (a b) (equal a b))

(defun parse-condition (condition)
  "A condition is of the form (parm inst op val).
  So for (age patient is 21), we would return 4 values:
  'age', 'patient-1', 'is' and '21', where patient-1
  is the current patient."
  (values
   (first condition)
   (get-db (second condition))
   (third condition)
   (fourth condition)))

(defun emycin (contexts)
  "An Expert-System Shell. Accumulate data for instances of each
  context, and solve for goals. Then report the findings."
  (clear-db)
  (get-context-data contexts))

(defun get-context-data (contexts)
  "For each context, create an instance and try to find out
  required data. Then go on to other contexts, depth-first,
  and finally, ask if there are other instances of this context."
  (unless (null contexts)
    (let* ((context (first contexts))
	   (inst (new-instance context)))
      (put-db 'current-rule 'initial)
      (mapc #'find-out (context-goals context))
      (report-findings context inst)
      (get-context-data (rest contexts))
      (when (y-or-n-p "Is there another ~a?"
		      (context-name context))
	(get-context-data contexts)))))


