;;;; paip/ch-14a.lisp
                    
;;;; Continuation of Chapter 14: Fixing
;;;; the Completeness issue.

(in-package :reasoning-completeness)
            
#| I separated this out because it 
involves rewriting our Prolog implementation.
Thus, I must put the prolog functions
that are not rewritten, interspersed
with the new functions. Finally, the 
functions from the beginning of this 
chapter.
|#
 
(defconstant +fail+ nil ; <- fail
  "Indicates pat-match failure")

(defconstant +no-bindings+ '((t . t))
  "Indicates pat-match success, with no
  variables") ; no-bindings
  
(defun variablep (x) ; variable-p
  "Is x a variable (a symbol beginning
  with '?')?"
  (and (symbolp x)
       (eql  ; <- equal
         (char
           (symbol-name x)
           0)
         #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a
  binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a 
  binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding
  list."
  (cons (cons var val) 
    ;; Once we add a "real" binding,
    ;; we can get rid of the dummy
    ;; +no-bindings+.
    (if (eq bindings +no-bindings+)
        ; <- (if (and (eq bindings +no-bindings+)))
        nil
        bindings)))

(defun match-variable (var input bindings)
  "Does var match bindings? Uses (or 
  updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond
      ((not binding)
       (extend-bindings var input bindings))
      ((equal input (binding-val binding))
       bindings)
      (t +fail+))))

(defun unify (x y &optional (bindings
                              +no-bindings+))
  ; 2
  "See if x and y match with given bindkngs."
  (cond
    ((eq bindings +fail+) +fail+)
    ((eql x y) bindings) 
    ;;**Moved the previous line
    ((variablep x)
     (unify-variable x y bindings))
    ((variablep y)
     (unify-variable y x bindings))
    ((and (consp x) (consp y))
     (unify
       (rest x)
       (rest y)
       (unify
         (first x)
         (first y)
         bindings)))
    (t +fail+)))
    
(defparameter *occurs-check* t
  "Should we do the occurs check?")
                           
(defun unify-variable (var x bindings)
  ; 3
  "Unify var with x, using (and maybe
  extending) bindings."
  (cond
    ((get-binding var bindings)
     (unify
       (lookup
         var
         bindings)
       x
       bindings))
    ((and (variablep x)
          (get-binding x bindings))
     (unify
       var
       (lookup x bindings)
       bindings))
    ((and *occurs-check*
          (occurs-check
            var
            x
            bindings))
     +fail+)
    (t
     (extend-bindings var x bindings))))
      
(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond
    ((eq var x)
     t)
    ((and (variablep x)
          (get-binding x bindings))
     (occurs-check var (lookup x bindings)
                       bindings))
    ((consp x)
     (or
       (occurs-check
         var
         (first x)
         bindings)
       (occurs-check
         var
         (rest x)
         bindings)))
    (t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in
  bindings into x, taking recursively 
  bound variables into account."
  (cond
    ((eq bindings +fail+) +fail+)
    ((eq bindings +no-bindings+) x)
    ((and (variablep x)
          (get-binding x bindings))
     (subst-bindings 
       bindings
       (lookup x bindings)))
    ((atom x) x)
    (t
      (reuse-cons 
        (subst-bindings 
          bindings
          (car x))
        (subst-bindings
          bindings
          (cdr x))
        x))))

(defun unifier (x y)
  "Return something that unifies with both
  x and y (or fail)."
  (subst-bindings (unify x y) x))
  
;; Clauses are represented as (head . body)
;; cons cells.

(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

;; Clauses are stored on the predicate's
;; plist

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defvar *db-predicates* nil)
  
(defun add-clause (clause) ; 2
  "Add a clause to the database, indexed
  by the head's predicate."
  ;; The predicate must be a non-variable
  ;; symbol
  (let ((pred 
          (predicate 
            (ensure-list
              (clause-head clause)))))
    (assert 
      (and
        (symbolp pred)
        (not (variablep pred))))
    (pushnew pred *db-predicates*)
    (setf
      (get pred 'clauses)
      (nconc 
        (get-clauses pred) 
        (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses (for all predicates)
  from the database."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single 
  predicate."
  (setf (get predicate 'clauses) nil))
  
;; new
(defvar *search-cutoff* nil
  "Has the search been stopped?")
  
;; reformulated

(defun prove-all (goals bindings depth)
  "Find a solution to the conjunction of
  goals."
  ;; This version just passes the depth
  ;; on to PROVE
  (cond
    ((eq bindings +fail+) ; <
     +fail+)
    ((null goals) bindings)
    (t 
      (prove 
        (first goals)
        bindings
        (rest goals)
        depth))))
        
;; reformulated

(defun prove (goal bindings other-goals depth)
  "Return a list of possible solutions to 
  goal."
  ;; Check if the depth bound has been 
  ;; exceeded
  (if (zerop depth)
      (progn
        (setf *search-cutoff* t)
        +fail+)
      (let ((clauses 
              (get-clauses 
                (predicate 
                  goal))))
        (if (listp clauses)
            (some
              #'(lambda (clause)
                  (let ((new-clause
                          (rename-variables  
                            clause)))
                    (prove-all
                      (append
                        (clause-body
                          new-clause)
                        other-goals)
                        (unify
                        goal
                        (clause-head new-clause)
                        bindings)
                      (1- depth))))
                clauses)
            ;; The presicate's 'clauses' 
            ;; can be an atom: a 
            ;; simple function to call
            (funcall 
              clauses
              (rest goal)
              bindings
              other-goals
              depth)))))
              
;; new

(defparameter *depth-start* 5  
  "The depth of the first round of iterative
  search.")
;; new

(defparameter *depth-incr* 5
   "Increase the depth of each iteration
   of the search by this amount.")
   
;; new

(defparameter *depth-max* 
  most-positive-fixnum
  "The deepest we will ever search.")   
  
;; reformulated
                                       
(defun top-level-prove (goals)
  (let ((all-goals
          `(,@goals  
             (show-prolog-vars
               ,@(variables-in 
                   goals)))))
    (loop 
      for depth from *depth-start* to *depth-max*
          by *depth-incr*
      while (let ((*search-cut-off* nil))
              (prove-all 
                all-goals
                +no-bindings+ ; <
                depth)
              *search-cut-off*)))
  (format t "~&No.")
  (values))

;; The following refactoring prevents 
;; duplicate printings of proofs
;; from shallower iterations

;; reformulated

(defun show-prolog-vars 
       (vars bindings other-goals depth)
  "Print each variable with its binding.
  Then ask the user if more solutions are
  desired."
  (if (> depth *depth-incr*)
      +fail+
      (let* ((bs 
                (make-broadcast-stream
                 *query-io*
                 *standard-output*))
             (*standard-output* bs))
        (if (null vars)
            (format t "~&Yes.")
            (dolist (var vars)
              (format t "~&~A = ~A" 
                var
                (subst-bindings bindings var)))
            (if (continuep)
                +fail+
                (prove-all 
                  other-goals
                  bindings
                  depth))))))
                
(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis
    (mapcar
      #'(lambda (var)
          (cons var (gensym (string var))))
        (variables-in x))
    x))

(defun variables-in (exp)
  "Return a list of all variables in the
  arg exp."
  (unique-find-anywhere-if #'variablep exp))
                
(defun unique-find-anywhere-if 
       (predicate tree &optional found-so-far)
  "Return a list of leaves of tree 
  satisfying predicate with duplicates
  removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if
          predicate
          (rest tree)
          found-so-far))))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the 
  solutions."
  (if (null solutions)
      (format t "~&No.")
      (mapc
        #'(lambda (solution)
            (show-prolog-vars vars solution))
          solutions))
  (values))
  
(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))
      
(setf (get 'show-prolog-vars 'clauses) 
      'show-prolog-vars)
      
(defun continuep ()
  "Ask user if we should continue looking
  for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continuep))
    (otherwise 
      (format t " Type ; to see more or ~%~
              . to stop.")
      (continuep))))
      
(eval-when (:compile-toplevel
            :load-toplevel
            :eval)
  (defun replace-?-vars (exp)
    "Replace any ? in exp with a var of the
    form #:?123."
    (cond
      ((eq exp '?) (gensym "?"))
      ((atom exp) exp)
      (t
        (reuse-cons
          (replace-?-vars (first exp))
          (replace-?-vars (rest exp))
          exp)))))

(defmacro <- (&rest clause) ; 2
  "Add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))
  
(defmacro ?- (&rest goals) ; 3
  "Make a query and print answers."
  `(top-level-prove ',(replace-?-vars goals)))

;; Now, the definitions from the beginning
;; of the chapter

(defun make-empty-nlist ()
  "Create a new, empty nlist."
  (cons 0 nil))
  
(defun nlist-n (nlist)
  "Return the number of elements in an
  nlist."
  (car nlist))

(defun nlist-list (nlist)
  "Return the elements in an nlist."
  (cdr nlist))

(defun nlist-push (item nlist)
  "Add a new element to an nlist."
  (incf (car nlist))
  (push item (cdr nlist)))
  
;; The database will be built out of 
;; nodes called dtree nodes. These will
;; be implemented as vectors, both for
;; efficiency, and because we will never
;; need a dtree-p predicate.

(defstruct (dtree (:type vector))
  (first nil)
  (rest nil)
  (atoms nil)
  (var (make-empty-nlist)))

;; A separate dtree will be stored for 
;; each predicate. We will store each
;; ptree on its head symbol's plist

(let ((predicates nil))
  (defun get-dtree (predicate)
    "Fetch (or make) the dtree for this
    predicate."
    (cond
      ((get predicate 'dtree))
      (t
        (push predicate predicates)
        (setf
          (get predicate 'dtree)
          (make-dtree)))))
  (defun clear-dtrees ()
    "Remove all the dtrees for all the
    predicates."
    (dolist (predicate predicates)
      (setf (get predicate 'dtree) nil))
    (setf predicates nil)))

;; Now we need a way to store the 
;; facts

(defun index (key)
  "Store key in a dtree node...key must
  be of the form (predicate . args); it
  is stored in the predicate's dtree."
  (dtree-index 
    key 
        (rename-variables key) ; store unique vars
    (get-dtree (predicate key))))
    
(defun dtree-index (key value dtree)
  "Index value under all atoms of key in 
  dtree."
  (cond
    ((consp key) ; index on both first and rest
     (dtree-index 
       (first key)
       value
       (or 
          (dtree-first dtree)
          (setf (dtree-first dtree) 
                (make-dtree))))
     (dtree-index
       (rest key)
       value
       (or
          (dtree-rest dtree)
          (setf (dtree-rest dtree)
                (make-dtree)))))
    ((null key)) ; don't index on nil
    ((variablep key) ; index a variable
     (nlist-push 
       value 
       (dtree-var dtree)))
    (t ;; make sure there is an nlist 
       ;; for this atom, and add to it
       (nlist-push
         value
         (lookup-atom key dtree)))))

(defun lookup-atom (atom dtree)
  "Return (or create) the nlist for this
  atom in dtree."
  (or (lookup atom (dtree-atoms dtree))
      (let ((new (make-empty-nlist)))
        (push (cons atom new)
              (dtree-atoms dtree))
        new)))
        
;; A testing function to see if what we
;; have so far works.

(defun test-index ()
  (let ((props '((p a b) (p a c) (p a ?x)
                 (p b c) (p b (f c))
                 (p a (f . ?x)))))
    (clear-dtrees)
    (mapc #'index props)
    (write
      (list props (get-dtree 'p))
      :circle t
      :array t
      :pretty t))
  (values))
  
;; Now, a function to fetch matches

(defun fetch (query)
  "Return a list of buckets potentially
  matching the query, which must be a 
  relation of the form (predicate . args)."
  (dtree-fetch 
    query 
    (get-dtree (predicate query))
    nil
    0
    nil
    most-positive-fixnum))

(defun dtree-fetch (pat 
                    dtree 
                    var-list-in 
                    var-n-in
                    best-list
                    best-n)
  "Return two values: a list of possible
  matches to pat, and the number of 
  elements in the list-of-lists."
  (if 
     (or 
        (null dtree) 
        (null pat)
        (variablep pat))
     (values best-list best-n)
     (let* ((var-nlist (dtree-var dtree))
            (var-n (+ var-n-in
                      (nlist-n var-nlist)))
            (var-list 
              (if 
                 (null 
                   (nlist-list var-nlist))
                 var-list-in
                 (cons
                   (nlist-list var-nlist)
                   var-list-in))))
       (cond
         ((>= var-n best-n)
          (values best-list best-n))
         ((atom pat)
          (dtree-atom-fetch 
            pat
            dtree
            var-list
            var-n
            best-list
            best-n))
         (t
           (multiple-value-bind
               (list1 n1)
               (dtree-fetch
                 (first pat)
                 (dtree-first dtree)
                 var-list
                 var-n
                 best-list
                 best-n)
             (dtree-fetch
               (rest pat)
               (dtree-rest dtree)
               var-list
               var-n
               list1
               n1)))))))

(defun dtree-atom-fetch (atom
                         dtree
                         var-list
                         var-n
                         best-list
                         best-n)
  "Return the answers indexed at this atom
  (along with the vars), or return the 
  previous best answer, if it is better."
  (let ((atom-nlist 
          (lookup 
            atom 
            (dtree-atoms dtree))))
    (cond
      ((or
         (null atom-nlist)
         (null (nlist-list atom-nlist)))
       (values var-list var-n))
      ((and
         atom-nlist
         (< (incf var-n (nlist-n atom-nlist))
            best-n))
       (values 
         (cons (nlist-list atom-nlist)
               var-list)
         var-n))
      (t
        (values best-list best-n)))))
        
(proclaim '(inline mapc-retrieve))

(defun mapc-retrieve (fn query)
  "For every fact that matches the query, 
  apply fn to the binding list."
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((bindings (unify query answer)))
        (unless (eq bindings +fail+)
          (funcall fn bindings))))))

(defun retrieve (query)
  "Find all facts that match query. 
   Return a list of bindings."
   (let ((answers nil))
     (mapc-retrieve
       #'(lambda (bindings)
           (push bindings answers))
         query)
     answers))

(defun retrieve-matches (query)
  "Find all facts that match query.
  Return a list of expressions that
  match the query."
  (mapcar #'(lambda (bindings)
              (subst-bindings bindings query))
            (retrieve query)))
            
(defmacro query-bind (variables 
                      query
                      &body body)
  "Execute body for each match to the 
  query. Within the body, bind each 
  variable."
  (let* ((bindings (gensym "BINDINGS"))
         (vars-and-vals
           (mapcar
             #'(lambda (var)
                 (list var 
                       `(subst-bindings
                         ,bindings
                         ',var)))
               variables)))
    `(mapc-retrieve
       #'(lambda (,bindings)
           (let ,vars-and-vals
             ,@body))
         ,query)))