;;;; 09-efficiency.lisp

;;;; Chapter 9: Efficiency Issues
                                
(in-package :efficiency)

;;; Memoization

(defun fib (n)
  "Computes the nth number in the 
  Fibonacci sequence. Simple but 
  horribly inefficient approach."
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun memo% (fn) ; 1
  "Given a function, return a memoized
  version of the function."
  (let ((table (make-hash-table)))
    (lambda (x)
      (multiple-value-bind
          (val foundp)
          (gethash args table)
        (if foundp
            val
            (setf
             (gethash args table)
             (funcall fn x)))))))

(defun memoize% (fn-name)
  "Given a function, reassign its name
  to a memoized version of the function."
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name))))

(defmacro defun-memo% (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args ,@body)))

(defun memo (fn-name &key (memo-name
                           fn-name) 
                          (key #'identity) 
                          (test #'equal))
  "Given a function name, return a memoized
  version of its function. Improved
  version. Kwargs include :memo-name
  the symbol that will hold the 
  memoization table as a property 
  value. Defaults to the supplied 
  function name. :key specifies
  which argument to use for keys in 
  the memo table. Defaults to #'identity, 
  meaning the complete list of args is 
  the key. :test is the hash-table test for
  the memo table. Defaults to #'equal, as
  must be the case when :key is #'identity."
  (let ((table (make-hash-table :test test))
        (old-fn (symbol-function fn-name)))
    (setf (get memo-name 'table) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind
              (val foundp)
              (gethash k table)
            (if foundp
                val
                (setf (gethash k table)
                  (apply 
                    old-fn args))))))))
  
(defun memoize (fn-name &key
                        (memo-name fn-name)
                        (key #'identity)
                        (test #'equal))
  "Given a function name, replace its
  function with a memoized version of 
  the function. Takes kwargs which are 
  passed to memo."
  (setf (symbol-function fn-name)
        (memo
         fn-name
         :memo-name memo-name
         :key key
         :test test)))

;;; Compiling one language into another
;;; Example: using a compiled version
;;; of the rules from 
;;; Chapter 2. We turn our rules into
;;; functions.

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun one-of (set)
  "Pick one element at random from a set
  and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Given a sequence, return a random element."
  (elt choices (random (length choices))))

(defun compile-rule (rule)
  "Translate a grammar rule into a CL 
  function definition."
  (let ((rhs (rule-rhs rule)))
    `(defun ,(rule-lhs rule) ()
       ,(cond ((every #'atom rhs)
               `(one-of ',rhs))
              ((length=1 rhs)
               (build-code (first rhs)))
              (t
               `(case (random ,(length rhs))
                  ,@(build-cases 0 rhs)))))))

(defun build-cases (number choices)
  "Return a list of case clauses."
  (when choices
    (cons
     (list number 
       (build-code 
         (first choices)))
     (build-cases 
       (1+ number) 
       (rest choices)))))

(defun build-code (choice)
  "Append together multiple constituents"
  (cond
   ((null choice)
    nil)
   ((atom choice)
    (list choice))
   (t
    `(append 
       ,@(mapcar #'build-code choice)))))

;; Two ways to use this code to build
;; rule functions. For debugging, use

;; (dolist (rule *grammar*)
;;   (eval (compile-rule rule)))
                               
;; After debugging, use

;; (dolist (rule *grammar*)
;;   (compile (eval (compile-rule rule))))

 
;; Here, we can define rules as functions,
;; rather than converting rules to 
;; functions

(defmacro defrule (&rest rule)
  "Define a grammar rule."
  (compile-rule rule))

#|

;; Compile compile-rule and its 
;; auxiliary functions before running this
;; code.
  
(defrule sentence -> (np vp))

(defrule np -> (art noun))

(defrule vp -> (verb np))

(defrule art -> the a)

(defrule noun -> man woman ball table)

(defrule verb -> hit took saw liked)
         
|#

;; Interpreters give us more flexibility
;; than compilers. Given the right setup, 
;; compilers give us the advantage of 
;; compile-time checks.

;; Snags: both versions lack the ability
;; to handle nouns of more than one word.
;; They also lack the ability to 
;; employ words that are category names
;; in sentences. Another issue with the
;; compiled version: name clashes. We 
;; could inadvertently redefine a CL
;; function. Competing grammars might be
;; incompatible, etc.

;; The main advantage of compiling our 
;; grammar is a radical increase in speed.

;;; Lazy evaluation

;; We might want to deal with infinite
;; sets. Of course, we won't use the whole
;; sets, but we do not know in advance 
;; how much of the set we will need. This
;; is a job for closures, and specifically,
;; delay objects.

(defstruct delay
  (value nil)
  (function nil))

(defmacro delay (&body body)
  "A computation that can be executed later
  by the ``force'' function."
  `(make-delay 
     :function #'(lambda () ,@body)))

(defun force (x)
  "Find the value of x, by computing it
  if it is a delay."
  (cond
   ((delay-p x)
    (when (delay-function x)
      (setf (delay-value x)
            (funcall (delay-function x))
            (delay-function x)
            nil))
    (delay-value x))
   (t x)))

#|

(setq x (list (print 1) (delay (print 2))))
printout-> 1
-> (1 #S(DELAY :VALUE NIL 
         :FUNCTION #<bytecompiled-function
                    9x727988a500>))
(force (second x))
printout-> 2
-> 2
|#

;; Let us get to work creating a lazy
;; sequence. We will call this type of
;; sequence a pipe.

(defmacro make-pipe% (head tail) ; 1
  "Create a pipe by evaluating head and
  delaying tail."
  `(cons ,head (delay ,tail)))

(defconstant *empty-pipe* nil)

(defun head (pipe)
  (first pipe))

(defun tail% (pipe) ; 1
  (force (rest pipe)))

(defun pipe-elt (pipe i)
  "The i'th element of a pipe, 0-based"
  (if (zerop i)
      (head pipe)
      (pipe-elt (tail pipe) (1- i))))

;; Example: a pipe of integers 
;; (a potentially infinite set)

(defun integers (&optional (start 0) end)
  "A pipe of integers. Optional args
  start defaults to 0. End defaults to
  nil. If end is nil, pipe is infinitely
  long."
  (if (or (not end)
          (<= start end))
      (make-pipe 
        start 
        (integers (1+ start) end))
      nil))
      
#|

(setq c (integers))

-> (0 . #S(DELAY 
            :VALUE NIL 
            :FUNCTION #<....>))
(pipe-elt c 0)
-> 0
c
-> (0 . #S(DELAY 
            :VALUE NIL 
            :FUNCTION #<....>))
(pipe-elt c 5)
-> 5
c
-> (0 . #S(DELAY :VALUE
    (1 . #S(DELAY :VALUE
      (2 . #S(DELAY :VALUE
        (3 . #S(DELAY :VALUE
          (4 . #S(DELAY :VALUE
            (5 . #S(DELAY :VALUE NIL :FUNCTION #< ... >))
            :FUNCTION NIL))
          :FUNCTION NIL))
        :FUNCTION NIL))
      :FUNCTION NIL))
    :FUNCTION NIL))     
                       
|#      

;; This works as advertised. But it is 
;; inefficient in both space and 
;; time. We can improve this by using
;; one closure per pipe, and avoiding
;; delays altogether.

;; Caution: the data produced must not
;; contain the symbol LAMBDA. If you
;; can trust that your functions are 
;; always compiled, you can avoid this
;; problem by using compiled-function-p
;; rather than functionp.

(defmacro make-pipe (head tail) ; 2
  "Create a pipe by evaluating head and
  delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(defun tail (pipe) ; 2
  "Return tail of pipe or list, and 
  destructively update the tail if it 
  is a function."
  (if (functionp (rest pipe))
      (setf (rest pipe)
            (funcall (rest pipe)))
      (rest pipe)))
      
#|

;; after recompiling

(setq c (integers))

-> (0 . #< ... >)
   
(pipe-elt c 0)
-> 0
c
-> (0 . #< ... >)
(pipe-elt c 5)
-> 5
c
-> (0 1 2 3 4 5 . #< ... >)
(setq c (integer 0 10))
-> (0 . #< ... >)
(pipe-elt c 11)
-> NIL
c
-> (0 1 2 3 4 5 6 7 8 9 10)
   
|#

;; Some pipe utility functions

(defun enumerate (pipe 
                  &key count key
                  (result pipe))
   "(enumerate pipe &key count key 
   (result pipe)).
   pipe: a pipe object
   count: non-negative integer or nil
   key: function of one arg
   result: an object. Goes through each
   element of pipe, or stops after :count
   is reached. Applies :key to each 
   element encountered. Returns :result."
   (cond
     ((or (eq pipe *empty-pipe*)
          (<= count 0))
      result)
     (t
      (when key
            (funcall key (head pipe)))
      (enumerate
        (tail pipe)
        :count (when count (1- count))
        :key key
        :result result))))

(defun filter (pred pipe)
  "Takes a predicate function and a 
  pipe object. Returns a pipe object 
  whose calculated contents contain
  only items that pass the predicate."
  (if (funcall pred (head pipe))
      (make-pipe 
        (head pipe)
        (filter pred (tail pipe)))
      (filter pred (tail pipe))))

;; Example application for filter.

(defun sieve (pipe)
  (make-pipe 
    (head pipe)
    (filter
     #'(lambda (x)
         (/= (mod x (head pipe)) 0))
       (sieve (tail pipe)))))

;; The following hack obviates the need
;; to put eval-when declarations around 
;; all the functions and the macro 
;; necessary to create a pipeline of 
;; primes. If I don't do one or the
;; other, the stack overflows when the 
;; package is first loaded in a 
;; session.

(defvar *primes* nil)

(defun populate-primes ()
  (setf *primes*
    (sieve (integers 2))))

;;; It is a neat technique. But we must
;;; realize that each successive prime
;;; found adds another filter to the 
;;; pipe. It doesn't take long before
;;; the stack overloads. I can access the
;;; 243rd prime, but not the 244th.
                  

#|

(enumerate *primes* :count 10)
-> (2 3 5 7 11 13 17 19 23 29 31
      . #< ...>)
   
|#

(defun map-pipe (fn pipe)
  "Takes a function of one arg and 
  a pipe object. Maps function over 
  the elements in the pipe, delaying
  all but the first function call."
  (if (eq pipe *empty-pipe*)
      *empty-pipe*
      (make-pipe
       (funcall fn (head pipe))
       (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  "Given two pipes, the first of which
  is presumably of finite length, 
  return a pipe that is the concatenation
  of the elements of the two pipes."
  (if (eq x *empty-pipe*)
      y
      (make-pipe
       (head x)
       (append-pipes (tail x) y))))
       
(defun mappend-pipe (fn pipe)
  "Given a function of one argument
  that produces a list and a pipe, 
  return a pipe which contains the
  appended lists produced by mapping
  the function over the elements in the
  pipe argument."
  (if (eq pipe *empty-pipe*)
      *empty-pipe*
      (let ((x (funcall fn (head pipe))))
        (make-pipe
         (head x)
         (append-pipes
          (tail x)
          (mappend-pipe fn (tail pipe)))))))

;; These functions are used in the text
;; to create a new version of 
;; generate-all from grammar chapter 
;; that is capable of listing elements
;; of an infinite set---the set of 
;; possible sentences produceable by
;; a recursive grammar. However, the
;; naive approach shown does not give 
;; a representative sampling of those
;; sentences.

;;; Indexing Data

;;; Instrumentation: Deciding what to 
;;; optimize. A very simple profiling
;;; utility is next 
                   


(defun profile1% (fn-name) ; 1
  "Make the function count how often
  it is called. The sole arg is the 
  quoted function name, NOT a function
  literal."
  ;; Save the old, unprofiled function, 
  ;; then set the function name to
  ;; both run the function and update
  ;; a counter.
  (let ((fn (symbol-function fn-name)))
    (setf
     (get fn-name :unprofiled-fn) fn
     (get fn-name :profile-count) 0
     (symbol-function fn-name)
       (profiled-fn fn-name fn))
    fn-name))

(defun unprofile1% (fn-name) ; 1
  "Given the quoted symbol 
  fbound to a profiled function, 
  restore the symbol's bunding
  to the original, unprofiled function."
  (setf (symbol-function fn-name)
        (get fn-name :unprofiled-fn))
  (remprop fn-name :unprofiled-fn)
  (remprop fn-name :profile-count)
  fn-name)

(defun profiled-fn% (fn-name fn) ; 1
  "Return a function that 
  increments the count."
  #'(lambda (&rest args)
      (incf (get fn-name :profile-count))
      (apply fn args)))

(defun profile-count (fn-name)
  (get fn-name :profile-count))

(defun profile-report% (fn-names 
                       &optional
                       (key #'profile-count))
  "Report profiling statistics on given
  functions." ; 1
  (dolist (name (sort 
                  fn-names 
                  #'>
                  :key key))
    (format t "~&~7D ~A"
      (profile-count name) name)))
      
#| 
  
The code above isn't satisfactory. First,
we want to keep track of the amount of 
time each function runs, not just the 
number of times it is called. We
also want to profile and keep track
of several functions at once. There are
also some snags:
     
1. If a user accidentally profiles a 
function twice, the report will be 
wildly inaccurate.
       
2. Every time we alter a function, we 
need to unprofile and reprofile.
     
|#

(defvar *profiled-functions* nil
  "Function names that are currently
  profiled.")

(defun profile1 (fn-name) ; 2
  "Make the function count how often
  it was called."
  ;; First save away the old, unprofiled
  ;; function, then make the name 
  ;; be a new function that increments
  ;; a counter and then calls the original
  ;; function.
  (let ((fn (symbol-function fn-name)))
    (unless (eq 
              fn 
              (get fn-name :profiled-fn))
      (let ((new-fn 
              (profiled-fn fn-name fn)))
        (setf
         (symbol-function fn-name) new-fn
         (get fn-name :profiled-fn) new-fn
         (get fn-name :unprofiled-fn) fn
         (get fn-name :profile-time) 0
         (get fn-name :profile-count) 0))))
  fn-name)

(defun unprofile1 (fn-name)
  "Make the function stop counting how
  often it is called."
  (setf (get fn-name :profile-time) 0
        (get fn-name :profile-count) 0)
  (when (eq (symbol-function fn-name)
            (get fn-name :profiled-fn))
    ;; Normal case: restore unprofiled version
    (setf (symbol-function fn-name)
          (get fn-name :unprofiled-fn)))
  fn-name)
 
(defmacro profile (&rest fn-names)
  "Profile the supplied function
  names. With no args, returns a list
  of profiled functions."
  `(mapcar #'profile1
     (setf *profiled-functions*
       (union *profiled-functions*
              ,fn-names))))

(defun get-fast-time ()
  "Return the elapsed time."
  (get-internal-real-time))
  
(defun fast-time-difference (end start)
  "Subtract two time points."
  (- end start))
  
(defun fast-time->seconds (time)
  "Convert a fast-time interval to 
  seconds."
  (float (/ time internal-time-units-per-second)
         0d0))

(defmacro unprofile (&rest fn-names)
  "Stop profiling supplied fn-names. With
  no args, stop profiling all functions."
  `(progn
     (mapcar #'unprofile1
       ,(if fn-names
            fn-names
            '*profiled-functions*))
     (setf *profiled-functions*
       ,(if fn-names
            `(set-difference
             *profiled-functions*
             ',fn-names)
            nil))))
            

(proclaim '(inline profile-enter
                   profile-exit
                   inc-profile-time))

(defun profiled-fn (fn-name fn)
  "Return a function that increments 
  the count, and times."
  #'(lambda (&rest args)
      (profile-enter fn-name)
      (multiple-value-prog1
       (apply fn args)
       (profile-exit fn-name))))

(defvar *profile-call-stack* nil)

(defun profile-enter (fn-name)
  (incf (get fn-name :profile-count))
  (when *profile-call-stack*
    ;; time charged against the calling
    ;; function
    (inc-profile-time
     (first *profile-call-stack*)
     (car (first *profile-call-stack*))))
  ;; put a new entry on the stack
  (push
   (cons
    fn-name
    (get-fast-time))
   *profile-call-stack*))

(defun profile-exit (fn-name)
  ;; Time charged against the current
  ;; function
  (inc-profile-time
   (pop *profile-call-stack*)
   fn-name)
  ;; Change the top entry to reflect the
  ;; current time
  (when *profile-call-stack*
    (setf 
      (cdr
       (first *profile-call-stack*))
      (get-fast-time))))

(defun inc-profile-time (entry fn-name)
  (incf
   (get fn-name :profile-time)
   (fast-time-difference 
     (get-fast-time)
     (cdr entry))))

(defun profile-report 
       (&optional 
          (fn-names 
            (copy-list *profiled-functions*))
          (key #'profile-count))
  "Report profiling statistics on given
  functions."
  (let ((total-time
         (reduce #'+ 
           (mapcar #'profile-time fn-names))))
    (when key
      (setf fn-names
        (sort fn-names #'> :key key)))
    (format t "~&Total elapsed time: ~D seconds."
      (fast-time->seconds total-time))
    (format t "~&Count Secs Time% Name")
    (loop
      for name in fn-names
      do
      (format t "~&~7D ~6,2F ~A% ~A"
        (profile-count name)
        (fast-time->seconds 
          (profile-time name))
        (if (zerop total-time)
            'nan
            (round
              (/ (profile-time name)
                 total-time)
                 0.01))
        name))))

(defun profile-time (fn-name)
  (get fn-name :profile-time))

(defmacro with-profiling (fn-names &body body)
  `(progn
     (unprofile ,@fn-names)
     (profile ,@fn-names)
     (setf *profile-call-stack* nil)
     (unwind-protect
      (progn ,@body)
      (profile-report ,fn-names)
      (unprofile ,@fn-names))))

;;; Revisiting the SIMPLIFY program as an
;;; efficiency test-case

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
  (mapcar #'simplify *test-data*))

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
    
#|

(test-it)

[printout]
Total elapsed time: 0.241422d0 seconds.
Count Secs Time% Name
  50070   0.18 73% PAT-MATCH
  35424   0.03 11% VARIABLEP
   1317   0.00 1%  MATCH-VARIABLE
    856   0.00 1%  SIMPLIFY
    258   0.03 14% SIMPLIFY-EXPRESSION

Based on the number of calls, pat-match
and variablep take up the lion's share
of calls.

We can use memoization to trim the number
of calls.

It turns out, out of the 8 possibilities, 
using an equal hash table and not 
clearing it between tests produces the 
best result.

In addition to memoization, we can 
employ indexing by operator in our
rules to cut down on calls.

|# 
  
