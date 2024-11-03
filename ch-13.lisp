;;;; paip/ch-13.lisp

;;;; Chapter 13: Object-Oriented Programming
                                           
(in-package :object)
            
;; program representing a bank
;; account, written in procedural
;; style

(defstruct account%
  (name "")
  (balance 0.0)
  (interest-rate 0.06))

(defun account-withdraw% (account amt)
  "Make a withdraw from this account."
  (if (<= amt (account-balance account))
      (decf (account-balance account) amt)
      "Insufficient funds."))

(defun account-deposit% (account amt)
  "Make a deposit to an account."
  (incf (account-balance account) amt))

(defun account-interest% (account)
  "Accumulate interest in this account."
  (incf (account-balance account)
        (*
          (account-interest-rate account)
          (account-balance account))))
          
#| This works as advertised. But there
are some potential problems:
    
1. What if we need to change the 
specification? We will need to start
from scratch.
     
2. The system is prone to misuse. A user 
can bypass the dedicated withdraw function
allowing him to withdraw an arbitrary 
amount.

3. If we develop a new kind of account
that has a limit on withdrawals, our 
current program will not enforce this
limit.

|#

;; Program rewritten in an object-
;; oriented style, but not employing
;; CLOS.

(defun new-account (name &optional
                    (balance 0.00)
                    (interest-rate 0.06))
  "Create a new account that knows the
  following messages."
  #'(lambda (message)
      (case message
        (withdraw #'(lambda (amt)
                      (if (<= amt balance)
                          (decf balance amt)
                          "Insufficient funds.")))
        (deposit #'(lambda (amt)
                     (incf balance amt)))
        (balance #'(lambda () balance))
        (name #'(lambda () name))
        (interest #'(lambda ()
                      (incf balance
                        (* balance rate)))))))
                         
;; This improves security immensely.
;; Details of the account are now 
;; only accessible through the message-
;; passing system.
                 
(defun get-method (object message)
  "Return the method that implements
  the message for this object."
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the
  message and apply the function to 
  the args."
  (apply (get-method object message) args))

#|
 
(setq acct (new-account "J. Random Customer"
                        1000.00))
=> #<byte-compiled-closure 
     #<byte-compiled-function 0x76c5fa5820>>
(send acct 'withdraw 500.00)
=> 500.00
(send acct 'deposit 123.45)
=> 623.45
(send acct 'name)
=> "J. Random Customer"
(send acct 'balance)
=> 623.45
|#

#| It works! But it is awkward to use, and
is especially awkward to mix it in with
regular Lisp code. We need to implement
a more familiar interface. We do this with
generic functions. We can also create 
macros to formalize the concept of class. |#

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defun make-clause (clause)
    "Translate a clause from define-class
     to a case clause."
    `( ,(first clause) #'(lambda 
                           ,(second clause)
                           ,@(cddr clause)))))

(defun generic-fn-p (fn-name)
  "Is this a generic function?"
  (and (fboundp fn-name)
       (eq (symbol-function fn-name)
           (get fn-name 'generic-fn))))

(defun ensure-generic-fn (message)
  "Define an object-oriented dispatch
  function for a message, unless it has
  already been defined as one."
  (unless (generic-fn-p message)
    (let ((fn #'(lambda (object &rest args)
                  (apply 
                    (get-method object message)
                    args))))
      (setf (symbol-function message) fn
            (get message 'generic-fn) fn))))

(defmacro define-class 
          (class inst-vars class-vars
           &body methods)
  "Define a class for object-oriented
  programming."
  ;; Define constructor and generic functions
  ;; for methods
  `(let ,class-vars
     (mapcar 
       #'ensure-generic-fn
       ',(mapcar #'first methods))
     (defun ,class ,inst-vars
       #'(lambda (message)
           (case message
             ,@(mapcar 
                 #'make-clause
                 methods))))))

;;; And now an example

(define-class account%
              (name &optional (balance 0.00))
              ((interest-rate 0.06))
  (withdraw% (amt) 
    (if (<= amt balance)
        (decf balance amt)
        'insufficient-funds))
  (deposit% (amt)
    (incf balance amt))
  
  (name% () name)
  (balance% () balance)
  (interest% () 
    (incf balance (* interest-rate balance))))               

#|

(setq acct2 (account "A. Customer" 2000.00))
=> #<byte-compiled-closure 
     #<byte-compiled-function 0x6f2e766b40>>
(deposit acct2 42.00)
=> 2042.00
(interest acct2)
=> 2164.14
(balance acct2)
=> 2164.14
         
now, we go back to our prior account:
                                    
(balance acct)
=> 623.45
   
;; The new methods work on accounts
;; created with the old implementation
                                     
;; Next, let's create a new account class
;; that requires a password.

|#
 
;; Not such a good implementation: it
;; relies on the inner workings of 
;; define-class: It uses otherwise,
;; since these methods will be part
;; of a case expression, and it uses
;; the message variable, which appears
;; in the macro, not here.

#|
(define-class password-acct (password acct)
                             ()
  (change-password (pass new-pass)
    (if (equal pass password)
        (setf password new-pass)
        'wrong-password))
  (otherwise (pass &rest args)
    (if (equal pass password)
        (apply message acct args)
        'wrong-password)))

Alas, this does work, because we are passing
 'otherwise' to ensure-generic-function, 
 and the package lock forbids rebinding
 cl native symbols. This could work with
 shadowing, but that would be too much 
 of a mess for a simple demonstration.
 Note that the acct argument would be 
 an already instantiated account instance.
 Another "limited-acct" class was defined
 that adopted the same strategy.
 
Inheritance is described.
            
And then we move on to CLOS.
    
|#

;; Here is the account class defined with
;; CLOS

(defclass account ()
  ((name :initarg :name :reader name)
   (balance 
     :initarg :balance 
     :initform 0.00
     :accessor balance)
   (interest-rate
     :allocation :class
     :initform 0.06
     :reader interest-rate)))

(defmethod withdraw ((acct account) amt)
  (if (<= amt (balance acct))
      (decf (balance acct) amt)
      'insufficient-funds))
      
;; Inheritance

(defclass limited-account (account)
  ((limit :initarg :limit :reader limit)))
  
;; New method, employs "call-next-method"

(defmethod withdraw ((acct limited-account)
                     amt)
  (if (> amt (limit acct))
      'over-limit
      (call-next-method)))
#|

(setq a1 (make-instance 'account
           :balance 5000.00
           :name "Fred"))
=> #<a OBJECT::ACCOUNT 0x6f2f762a00>
(setq a2 (make-instance 'limited-account
           :name "A. Thrifty Spender"
           :balance 500.00
           :limit 100.00))
=> #<a OBJECT::LIMITED-ACCOUNT 0x720cd28dc0>
(withdraw a2 200.00)
=> OVER-LIMIT
(withdraw a2 20.00)
=> 480.00
|#

(defclass audited-account (account)
  ((audit-trail :initform nil
    :accessor audit-trail)))
    
;; :before method

(defmethod withdraw :before ((acct audited-account)
                             amt)
  (push (print `(withdrawing ,amt))
                (audit-trail acct)))
                
;; :after method
  
(defmethod withdraw :after ((acct audited-account)
                            amt)
  (push (print `(withdrawal (,amt) done))
                (audit-trail acct)))

#|
(setq a3 (make-instance 'audited-account
           :balance 1000.00))
=> #<a OBJECT::AUDITED-ACCOUNT 0x6f2f1eed00>
(withdraw a3 100.00)
printout=> (WITHDRAWING 100.00)
printout=> (WITHDRAWAL (100.00) DONE)
=> 900.00
(audit-trail a3)
=> ((WITHDRAWAL (100.00) DONE) (WITHDRAWING 100.00))
(setf (audit-trail a3) nil)
=> NIL
   
|#
 
;;;; A CLOS Example: Searching Tools

(defconstant +fail+ nil)

(defclass problem ()
  ((states :initarg :states 
           :accessor problem-states)))

(defmethod searcher ((prob problem))
  "Find a state that solves the search
  problem."
  (cond
   ((no-states-p prob)
    (dbg 'search "~&;; Search: No solutions found.") 
    +fail+)
   ((goal-p prob) 
    (let ((cs (current-state prob)))
      (dbg 'search "~&;; Search: Solution found: ~A" cs)
      cs))
   (t
    (let ((current (pop-state prob)))
      (dbg 'search "~&;; Search: current: ~A" current)
      (setf (problem-states prob)
            (problem-combiner
              prob
              (problem-successors 
                prob
                current)
              (problem-states prob)))
      (dbg 
        'search 
        "~&;; Search: problem-states: ~A" 
        (problem-states prob)))
    (searcher prob))))

(defmethod current-state ((prob problem))
  "The current state is the first of 
  possible states."
  (first (problem-states prob)))

(defmethod pop-state ((prob problem))
  "Remove and return the current state."
  (pop (problem-states prob)))

(defmethod no-states-p ((prob problem))
  "Are there any more unexplored states?"
  (null (problem-states prob)))

;; add debugging

(defmethod searcher :before ((prob problem))
  (dbg 'search "~&;; Search: ~a"
       (problem-states prob)))

(defclass eql-problem (problem)
  ((goal :initarg :goal :reader problem-goal)))
  
(defmethod goal-p ((prob eql-problem))
   (eql (current-state prob)
        (problem-goal prob)))    
                                
(defclass dfs-problem (problem) ()
  (:documentation
   "Depth-first search problem."))

(defclass bfs-problem (problem) ()
  (:documentation
   "Breadth-first search problem."))

(defmethod problem-combiner ((prob dfs-problem)
                             new old)
  "Depth-first searches look at new 
  states first."
  (append new old))

(defmethod problem-combiner ((prob bfs-problem)
                            new old)
  "Breadth-first searches look at old 
  states first."
  (append old new))

(defclass binary-tree-problem (problem) ())

(defmethod problem-successors ((prob binary-tree-problem)
                               state)
  (let ((n (* 2 state)))
    (list n (1+ n))))

;; Creating a class to perform a search
;; for 12, starting with '(1), and 
;; employing bfs.

(defclass binary-tree-eql-bfs-problem
  (binary-tree-problem eql-problem bfs-problem)
  ())
  
#|

(setq p1 (make-instance 
           'binary-tree-eql-bfs-problem
           :states '(1)
           :goal 12))
=> #<a OBJECT::BINARY-TREE-EQL-BFS-PROBLEM 0x7004f6ebc0>
(searcher p1)
=> 12
[need to redefine the object to see the trace of the solution]
(debug* 'search)
=> (SEARCH)
(searcher p1)
[printout]
-> ;; Search: (1)
-> ;; Search: current: 1
-> ;; Search: problem-states: (2 3)
-> ;; Search: current: 2
-> ;; Search: problem-states: (3 4 5)
.
.
.
-> ;; Search: current: 10
-> ;; Search: problem-states: (11 12 13 14 15 16 17 18 19 20 21)
-> ;; Search: current: 11
-> ;; Search: problem-states: (12 13 14 15 16 17 18 19 20 21 22 23)
-> ;; Search: Solution found: 12
[end printout]
=> 12
   
|#                                                            

(defclass best-problem (problem) ()
  (:documentation 
   "A best-first-search problem."))

(defmethod problem-combiner ((prob best-problem)
                             new old)
  "Best-first search sorts new and old 
  according to cost-fn."
  (sort 
    (append new old) 
    #'<
    :key #'(lambda (state) 
             (cost-fn prob state))))
             
(defmethod cost-fn ((prob eql-problem)
                    state)
  (abs (- state (problem-goal prob))))

(defclass beam-problem (problem)
  ((beam-width 
    :initarg :beam-width 
    :initform nil
    :reader problem-beam-width))
  (:documentation
   "A beam-search problem."))

(defmethod problem-combiner :around ((prob beam-problem)
                                     new old)
  "Beam search keeps only a few of the 
  best results on each iteration."
  (let ((combined (call-next-method)))
    (subseq 
      combined
      0
      (min
        (problem-beam-width prob)
        (length combined)))))
        
;;; A beam-width search for 12 given (1)

(defclass binary-tree-eql-best-beam-problem
  (binary-tree-problem 
   eql-problem
   best-problem
   beam-problem)
  ())
  
#|
[debug* is still set from before]
(setq p3 (make-instance 
           'binary-tree-eql-best-beam-problem
           :states '(1)
           :goal 12
           :beam-width: 3))
-> #<a OBJECT::BINARY-TREE-EQL-BEST-BEAM-PROBLEM 0x712623ee40>
(searcher p3)
[printout]
-> ;; Search: (1)
-> :: Search: current: 1
-> ;; Search: problem-states: (3 2)
-> ;; Search: current: 3
-> ;; Search: problem-states: (7 6 2)
-> ;; Search: (7 6 2)
-> ;; Search: current: 7
-> ;; Search: problem-states: (14, 15, 6)
-> ;; Search: (14, 15, 6)
-> ;; Search: current: 14
-> ;; Search: problem-states: (15, 6, 28)
...
...
-> ;; Search: problem-states: (6 28 30)
...
...
-> ;; Search: problem-states: (12 13 28)
...
-> ;; Search: Solution found: 12
[End printout]
=> 12
|#


                      
                             
   
        