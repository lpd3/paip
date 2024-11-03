;;;; paip/ch-14.lisp
                   
;;;; Chapter 14: Knowledge Representation
;;;; and Reasoning

(in-package :reasoning)

#| We wish to create a knowledge base. Our
approach is to extend our Prolog implementation
to do so. We begin by addressing three 
snags that make pure Prolog a problemmatic
choice for this task.
       
The simplest of these snags is Indexing. 
Prolog handles simple and compound facts
ok. It is not adept at handling tabular 
facts: searches now proceed linearly 
through all facts in a table. Moreover,
if both queries and facts contain
wildcard variables, we can get strange 
answers.
        
The first thing to do is limit the search
space. We do this by devising a listing
of facts, where each fact appears under
a column for each atomic position in the
fact.
|#

;; An nlist is implemented as a (count . 
;; elements) pair.
 
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

(defun index% (key)
  "Store key in a dtree node. Key 
  must be (predicate . args); it
  is stored in the predicate's dtree."
  (dtree-index 
    key 
    key 
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

#|
(test-index)
[printout]
"((#1=(P A B) #2=(P A C) #3=(P A ?X) #4=(P B C) #5=(P B (F C))
  #6=(P A (F . ?X)))
 #(#(NIL NIL ((P 12 #6# #5# #4# #3# #2# #1# #6# #5# #4# #3# #2# #1#)) (0))
   #(#(NIL NIL ((B 4 #5# #4# #5# #4#) (A 8 #6# #3# #2# #1# #6# #3# #2# #1#))
       (0))
     #(#(#(NIL NIL ((F 4 #6# #5# #6# #5#)) (0))
         #(#(NIL NIL ((C 2 #5# #5#)) (0)) #(NIL NIL NIL (0)) NIL (2 #6# #6#))
         ((C 4 #4# #2# #4# #2#) (B 2 #1# #1#)) (2 #3# #3#))
       #(NIL NIL NIL (0)) NIL (0))
     NIL (0))
   NIL (0)))"
|#

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

#| If test-index is run and then code is 
changed, values will remain in the plists,
but the predicates private variable will
be erased. When you run test-index again,
the plists will contain double entries.
    
(test-index)
[same printout as before]
(fetch '(p ? c))
=> (((P B C) (P A C) (P A ?X)))
=> 3
   
|#

;; OK. Now we need to integrate this with
;; Prolog.

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

;; In our Prolog, the prove function
;; renames the variables in each clause
;; as they are retrieved. But here, we are
;; dealing with tabular data. We need to 
;; rename only once. So we refactor the 
;; index function for renaming. 
                  
(defun index (key)
  "Store key in a dtree node...key must
  be of the form (predicate . args); it
  is stored in the predicate's dtree."
  (dtree-index 
    key 
    (rename-variables key) ; store unique vars
    (get-dtree (predicate key))))

#|

[Reload packages]
(test-index)
[similar printout as before]
         
(fetch '(p ?x c))
=> (((P B C) (P A C)) ((P A #:?X135)))
=> 3
   
(retrieve '(p ?x c))
=> (((#:?X135 . C) (?X . A)) ((?X . A) (?X . B)))
   
(retrieve-matches '(p ?x c))
=> ((P A C) (P A C) (P B C))
   
(retrieve-matches '(p ?x (?fn c)))
=> ((P A (?FN C)) (P A (F C)) (P B (F C)))
   
|#       
        
;; Now, we write a macro that binds a query
;; and allows arbitrary code to be run with
;; the query.

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

#|

(query-bind (?x ?fn) '(p ?x (?fn c))
  (format t "~&P holds between ~A and ~A of C."
          ?x ?fn))
[printout]
P holds betweeb B and F of C.
P holds between A and F of C.
P holds between A and ?FN of C.
  
(macroexpand-1 
  '(query-bind (?x ?fn) '(p ?x (?fn c))
     (format t "~&P holds between ~A and ~A of C."
            ?x ?fn)))

=> (MAPC-RETRIEVE
    #'(LAMBDA (#:BINDINGS141)
        (LET ((?X (SUBST-BINDINGS #:BINDINGS141 '?X))
              (?FN (SUBST-BINDINGS #:BINDINGS141 '?FN)))
          (FORMAT T "~&P holds between ~A and ~A of C." ?X ?FN)))
     '(P ?X (?FN C)))
   
|#
 
#| Ok. The indexing snag has been addressed.
The next snag is the problem of completeness.
The issue is that Prolog searches are always
depth-first. In infinite (or merely huge)
solution spaces, there are 3 "subsnags."
(1) Search endlessly pursues a single branch
but the solution is found in a different 
branches. (2) There are infinitely many
solutions. (3) A relation or query may 
be infinitely recursive on its own. The
last has been countered with an 
 occurs check, but it should be mentioned
 that there are other ways around the 
 problem.
 |#
 
; we continue with this in the next file
  

                                                               
     



       

         



