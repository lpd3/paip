;;;; paip/ch-12.lisp

;;;; Chapter 12: Compiling Logic Problems

(in-package :compile-logic)

#| To make our project more efficient, 
we transition from an interpreter to 
compiler. Instead of storing clauses 
directly in a symbol, we convert the 
clause into a compiled function to 
be called on queries, and store these
 functions in the symbol. We will 
actually have the potential for 
several different functions on a single
symbol, each with a different arity.
|#

(defun prolog-compile 
       (symbol 
        &optional 
        (compilep t)
        (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function
  for each arity."
  (unless (null clauses)
    (let ((arity 
            (relation-arity 
              (clause-head 
                (first clauses)))))
      ;; Compile one clause with this arity
        (print (compile-predicate
                 symbol
                 arity
                 compilep
                 (clauses-with-arity
                 clauses
                 #'=
                 arity)))
      ;; Compile all the clauses with any
      ;; other arity
      (let 
        ((cl-without-ar
           (clauses-with-arity
             clauses
             #'/=
             arity)))
        (if cl-without-ar
            (print 
              (prolog-compile
               symbol
               compilep
               cl-without-ar))
            (prolog-compile
              symbol
              compilep
              cl-without-ar))))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given 
  arity."
  (find-all
    arity
    clauses
    :key #'(lambda (clause)
             (relation-arity
               (clause-head clause)))
    :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation."
  (length (args relation)))

(defun args (x)
  "The arguments of a relation"
  (rest x))

(defun compile-predicate% 
       (symbol arity clauses) ; 1
  "Compile all the clauses for a given
  symbol/arity into a single function."
  (let ((predicate
          (make-predicate symbol arity))
        (parameters
          (make-parameters arity)))
    (compile
      (eval
        `(defun ,predicate (,@parameters cont)
           ,@(mapcar
               #'(lambda (clause)
                   (compile-clause
                     parameters
                     clause
                     'cont))
                 clauses))))))

(defun make-parameters (arity)
  "Return the list (?arg, ?arg2, ..., ?argn)
  for arity."
  (loop for i from 1 to arity
        collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol <symbol>/<arity>"
  (concat-symbol symbol '/ arity))
  
#| Since we will be performing 
destructive operations in our backtracking,
it would seem the head must stay in 
contact with the body. A simpler approach
is to refactor the clauses, getting rid
of the head altogether. |#

(defun compile-clause% (parms clause cont) ;1
  "Transform away the head, and compile
  the resulting body."
  (compile-body
    (nconc
      (mapcar
        #'make-=
        parms
        (args (clause-head clause)))
      (clause-body clause))
    cont))
    
(defun make-= (x y)
  `(= ,x ,y))
  
(defun compile-body (body cont)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let* ((goal (first body))
             (macro
               (prolog-compiler-macro
                 (predicate goal)))
             (macro-val
               (when macro
                 (funcall 
                   macro
                   goal
                   (rest body)
                   cont))))
        (if (and macro 
                 (not 
                   (equal 
                     macro-val 
                     :pass)))
            macro-val
            (compile-call
              (make-predicate
                (predicate goal)
                (relation-arity goal))
              (mapcar
                #'(lambda (arg)
                    (compile-arg arg))
                (args goal))
              (if (null (rest body))
                  cont
                  `#'(lambda ()
                       ,(compile-body
                          (rest body)
                          cont))))))))

(defun compile-call (predicate args cont)
  "Compile a call to a Prolog predicate."
  `(,predicate ,@args ,cont))
                          
#| We need to have = as a primitive. 
Since we may wish to add other 
primitives later, and wish to separate
primitives from clauses or queries, we
set up a means of defining primitives
and store them at a different key on the
symbol plist. |#

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a 
  Prolog predicate."
  ;; NOTE: "name" is the raw name, not
  ;; <name>/<arity>
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro 
          (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist ,@body)))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass
        `(if ,(compile-unify 
                (first args)
                (second args))
             ,(compile-body body cont)))))
             
#| The following definitions are from the 
end of the previous chapter, and provide
destructive backtracking |#

(defconstant +unbound+ "Unbound")

(defvar *var-counter* 0)

(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*))
  (binding +unbound+))

(defun bound-p (var)
  (not (eq (var-binding var) +unbound+)))

(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn
     (loop 
       while (and (var-p ,exp)
                  (bound-p ,exp))
       do (setf ,exp (var-binding ,exp)))
     ,exp))
     
(defvar *trail* 
  (make-array 200
    :fill-pointer 0
    :adjustable t))

(defun set-binding! (var value)
  "Set var's bindinv to value, after 
  saving the variable in the trail.
  Always returns T."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)
  
(defun undo-bindings! (old-trail)
  "Undo all bindings up to a given
  point in the trail."
  (loop
    until (= (fill-pointer *trail*) 
             old-trail)
    do
    (setf (var-binding (vector-pop *trail*))
          +unbound+)))

(defun unify! (x y)
  "Destructively unify two expressions."
  (cond
    ((eql (deref x) (deref y)) t)
    ((var-p x) (set-binding! x y))
    ((var-p y) (set-binding! y x))
    ((and (consp x) (consp y))
     (and (unify! (first x) (first y))
          (unify! (rest x) (rest y))))
    (t nil)))

(defun compile-unify (x y)
  "Return code that tests if x and y
  unify."
  `(unify! 
     ,(compile-arg x)
     ,(compile-arg y)))
     
(defun compile-arg% (arg) ; 1
  "Generate code for an argument to a 
  goal in the body."
  (cond
    ((variablep arg) arg)
    ((not (has-variable-p arg)) `',arg)
    ((proper-list-p arg)
     `(list ,@(mapcar #'compile-arg arg)))
    (t
     `(cons ,(compile-arg (first arg))
            ,(compile-arg (rest arg))))))

(defun find-if-anywhere (pred x)
  (cond
    ((funcall pred x)
     x)
    ((atom x)
     nil)
    (t
      (or (find-if-anywhere pred (first x))
          (find-if-anywhere pred (rest x))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the 
  expression x?"
  (find-if-anywhere #'variablep x))
  
#| Snags: 
1. We forgot to undo rejected bindings
2. We forgot to provision for *trail*
3. Local variables are bound without 
   being introduced
   
We need to make some changes |#
  
(defun compile-predicate 
       (symbol arity compilep clauses) ; 2
  "Compile all the clauses for a given
  symbol/arity into a single function."
  (let* ((predicate
           (make-predicate symbol arity))
         (parameters
          (make-parameters arity))
         (expr
           `(defun ,predicate (,@parameters cont)
              ,(maybe-add-undo-bindings ;**
                 (mapcar
                   #'(lambda (clause)
                       (compile-clause
                         parameters
                         clause
                         'cont))
                 clauses)))))
         (if compilep
             (compile (eval expr))
             expr)))

(defun compile-clause (parms clause cont) ;2
  "Transform away the head, and compile
  the resulting body."
  (bind-unbound-vars ;**
    parms            ;**
    (compile-body
      (nconc
        (mapcar
          #'make-=
          parms
          (args (clause-head clause)))
        (clause-body clause))
      cont)))
  
(defun maybe-add-undo-bindings 
       (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before
  we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `(let ((old-trail 
               (fill-pointer *trail*)))
         ,(first compiled-exps)
         ,@(loop 
             for exp in (rest compiled-exps)
             collect 
             '(undo-bindings! old-trail)
             collect exp))))
             
(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp 
  (besides the parameters), then bind 
  them to new vars."
  (let ((exp-vars 
          (set-difference
            (variables-in exp)
            parameters)))
    (if exp-vars
        `(let ,(mapcar 
                 #'(lambda (var)
                     `(,var (?)))
                   exp-vars)
           ,exp)
         exp)))

#| Room for improvement: we are now 
   creating and binding all vars, including
   those that only appear once. We could
   stop that by putting the (?) call inline.
|#

(defmacro <- (&rest clause)
  "Add a clause to the database."
  `(add-clause ',(make-anonymous clause)))

(defun compile-arg (arg) ; 2
  "Generate code for an argument to 
  a goal in the body." 
  (cond
    ((eq arg '?) '(?))
    ((variablep arg) arg)
    ((not (has-variable-p arg)) `',arg)
    ((proper-list-p arg)
     `(list ,@(mapcar #'compile-arg arg)))
    (t
     `(cons
        ,(compile-arg (first arg))
        ,(compile-arg (rest arg))))))

(defun make-anonymous 
       (exp &optional 
         (anon-vars
           (anonymous-variables-in exp)))
  "Replace variables that are used only
  once with ?."
  (cond
    ((consp exp)
     (reuse-cons
       (make-anonymous
         (first exp)
         anon-vars)
       (make-anonymous 
         (rest exp)
         anon-vars)
       exp))
    ((member exp anon-vars) '?)
    (t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that 
  occur only once in tree."
  (let ((seen-once nil)
        (seen-more nil))
    (labels ((walk (x)
               (cond
                 ((variablep x)
                  (cond
                    ((member x seen-once)
                     (setf 
                       seen-once
                       (delete
                         x
                         seen-once))
                     (push x seen-more))
                    ((member x seen-more)
                     nil)
                    (t
                     (push x seen-once))))
                 ((consp x)
                  (walk (first x))
                  (walk (rest x))))))
      (walk tree)
      seen-once)))

#| Now we need to make this user-friendly |#

(defvar *db-predicates* nil) 

(defvar *uncompiled* nil
  "Prolog symbols that have not been 
  compiled.")
  
(defun add-clause (clause)
  "Add a clause to the database, 
  indexed by the predicate of the head."
  ;; The predicate must be a non-variable
  ;; symbol.
  (let ((pred 
          (predicate 
            (clause-head clause))))
    (assert
      (and (symbolp pred)
           (not (variablep pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)
    (setf
      (get pred 'clauses)
      (nconc (get-clauses pred) 
             (list clause)))
    pred))

(defun top-level-prove (goals)
  "Prove the list of goals by compiling
  and proving it."
  ;; First redefine top-level query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? 
                (variables-in goals))))
    (add-clause `((top-level-query) 
                  ,@goals
                  (show-prolog-vars
                    ,@(mapcar
                        #'symbol-name
                        vars)
                    ,vars))))
  ;; Now run it.
  (run-prolog
    'top-level-query/0 
    #'ignore*)
  (format t "~&No.")
  (values))
  
(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with
  a given continuation."
  ;; First compile anything else that 
  ;; needs it.
  (prolog-compile-symbols) 
  ;; Reset the trail and variable counter
  (setf (fill-pointer *trail*) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure cont)))

(defun prolog-compile-symbols
       (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols. By 
  default, the list is all symbols that
  need it."
  (mapc #'prolog-compile symbols)
  (setf *uncompiled*
        (set-difference 
          *uncompiled*
          symbols)))
          
;;; At the top-level, the cont arg
;;; does not need to have any kind 
;;; of useable arg. So we create a 
;;; function to pass it which ignores
;;; its args and does nothing.

(defun ignore* (&rest args)
  (declare (ignore args))
  nil)
  
(defun show-prolog-vars (var-names vars cont)
  "Display the variables, and prompt the
  user to see if we should continue. If 
  not, return to the top level."
  (if (null vars)
      (format t "~&Yes.")
      (let ((*standard-output*
             (make-broadcast-stream
               *standard-output*
               *query-io*)))
        (loop for name in var-names
          do
          (format t "~&~A = ~A"
            name (deref-exp var)))))
  (if (continuep) ;<<
      (funcall cont)
      (throw 'top-level-prove nil)))

(defun deref-exp (exp)
  "Build something equivalent to EXP with
  variables dereferenced."
  (if (atom (deref exp)) ;<<
      exp
      (reuse-cons ;;<<
        (deref-exp (first exp))
        (detef-exp (rest exp))
        exp))) 

(defmacro ?- (&rest goals) ; 3
  "Make a query and print answers."
  `(top-level-prove ',(replace-?-vars goals)))


  
                           
  
                         
        


  
  


           
                
                         
  
           