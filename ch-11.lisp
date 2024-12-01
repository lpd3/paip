;;;; paip/ch-11.lisp
                   
;;;; Chapter 11: Logic Programming

(in-package :logic)

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

;; The unify functions is very similar to
;; the pat-match function of earlier
;; chapters.

(defun unify% (x y &optional (bindings
                              +no-bindings+))
  ;; 1
  "See if x and y match with given bindings'"
  (cond
    ((eq bindings +fail+) +fail+)
    ((variablep x)
     (unify-variable x y bindings))
    ((variablep y)
     (unify-variable y x bindings))
    ((eql x y) bindings)
    ((and (consp x) (consp y))
     (unify
       (rest x)
       (rest y)
       (unify
         (first x)
         (first y)
         bindings)))
    (t +fail+)))

(defun unify-variable% (var x bindings) ; 1
  "Unify var with x, using (and maybe
  extending) bindings."
  ;; Warning: buggy version
  (if (get-binding var bindings)
      (unify
        (lookup var bindings)
        x
        bindings)
      (extend-bindings var x bindings)))

#|

(unify '(?x + 1) '(2 + ?y))
=> ((?Y . 1)(?X . 2))
(unify '?x '?y)
=> ((?X . ?Y))
(unify '(?x ?x) '(?y ?y))
=> ((?Y . ?Y) (?X . ?Y))
   
ok. but:
    
(unify '(?x ?x ?x) '(?y ?y ?y))
[Stack overflow or infinite loop]
       
Once ?y gets bound to itself, the call to
UNIFY within UNIFY-VARIABLE leads to an 
infinite loop.
         
|#

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

#|

The examples that worked before continue 
to produce the same results. 
   
In addition:
   
(unify '(?x ?x ?x) '(?y ?y ?y))
=> ((?X . ?Y))
(unify '(?x ?y) '(?y ?x))
=> ((?Y . ?X)(?X . ?Y))
   
But...

(unify '(?x ?y a) '(?y ?x ?x))
[Stack overflow or infinite loop]
       
The problem has only been moved, not fixed.

We should not deal with bound varibles, 
but rather, their bindings.
    
|#

(defun unify-variable%% (var x bindings) ; 2
  "Unify var with x, using (and possibly
  extending) bindings"
  (cond
    ((get-binding var bindings)
     (unify 
       (lookup var bindings) 
       x 
       bindings))
    ((and (variablep x) ;**
          (get-binding x bindings)) ;**
     (unify ;**
       var ;**
       (lookup x bindings) ;**
       bindings)) ;**
    (t
      (extend-bindings var x bindings))))
     
#|
(unify '(?x ?y) '(?y ?x))
=> ((?X . ?Y))
(unify '(?x ?y a) '(?y ?x ?x))
=> ((?Y . A)(?X . ?Y))
   
good.

what about this:
     
(unify '?x '(f ?x))
=> ((?X F ?X))
   
But this is an infinitely recursive
expression. We either should find a way
to deal with such things, or (as is 
more common) forbid them.
     
Checking for the presence of a variable
in its own value is called the "occurs
check".
|#
 
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

(defmacro <-% (&rest clause) ; 1
  "Add a clause to the database."
  `(add-clause ',clause))
  

(defun add-clause% (clause) ; 1
  "Add a clause to the database, indexed
  by the head's predicate."
  ;; The predicate must be a non-variable
  ;; symbol
  (let ((pred 
          (predicate 
            (clause-head clause))))
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

(defun prove% (goal bindings) ; 1
  "Return a list of possible solutions to 
  goal."
  (mapcan 
    #'(lambda (clause)
        (let ((new-clause
                (rename-variables clause)))
          (prove-all
            (clause-body new-clause)
            (unify
              goal
              (clause-head new-clause)
              bindings))))
      (get-clauses (predicate goal))))

(defun prove-all% (goals bindings) ; 1
  "Return a list of solutions to the 
  conjunction of goals."
  (cond
    ((eq bindings +fail+) +fail+)
    ((null goals) (list bindings))
    (t
     (mapcan #'(lambda (goal-solution)
                 (prove-all
                   (rest goals)
                   goal-solution))
               (prove 
                 (first goals)
                 bindings)))))

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

(defmacro ?-% (&rest goals) ; 1
  `(prove-all ',goals +no-bindings+))

#|

(<- (likes kim robin))
=> LIKES
(<- (likes sandy lee))
=> LIKES
(<- (likes sandy kim))
=> LIKES
(<- (likes robin cats))
=> LIKES
(<- (likes sandy ?x) (likes ?x cats))
=> LIKES
(<- (likes kim ?x) (likes ?x lee) 
                   (likes ?x kim))
=> LIKES
(<- (likes ?x ?x))
=> LIKES
   
(?- (likes sandy ?who))
=> (((?WHO . LEE) (?WHO . KIM))
    ((#:?X1032 . ROBIN) (?WHO . #:?X1032))
    ((#:?X1036 . CATS) (#:?X1033 . CATS)
     (#:?X1032 . SANDY) (?WHO . #:?X1032))
    ((#:?X1041 . CATS) (#:?X1032 . #:?X1041)
     (?WHO .  #:?X1032))
    ((?WHO . SANDY)(#:?X1043 . SANDY)))
   
What does this mean? There are 6 "people"
whom Sandy likes.
     
1. Sandy likes Lee. This is a given.
2. Sandy likes Kim. This is a given.
3. Robin likes cats. Sandy likes 
   anyone who likes cats. Sandy likes
   Robin.
4. Everyone likes himself. Cats like
   themselves. Sandy likes anyone who
   likes cats. Sandy likes cats. 
   Sandy likes anyone who likes 
   cats. Sandy likes herself.
5. Everyone likes himself. Cats like
   themselves. Sandy likes anyone who
   likes cats. Sandy likes cats.
6. Everyone likes himself. Sandy likes
   herself.
   
Note that Sandy appears twice, because there
are two ways of proving she likes herself
given the facts.
      
Note also that this response was ugly and
difficult to read.
   
|#

(defmacro ?-%% (&rest goals) ; 2
  `(top-level-prove ',goals))

(defun top-level-prove% (goals) ; 1
  "Prove the goals, and print variables
  readably."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals +no-bindings+)))

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

(defun show-prolog-vars% (vars bindings) ; 1
  "Print each variable with its binding."
  (if (null vars)
      (format t "~&Yes.")
      (dolist (var vars)
        (format t "~&~A = ~A" 
          var
          (subst-bindings bindings var))))
  (princ ";"))

#|

with the same input facts as before
     
(?- (sandy likes ?who))
[printouts]
?WHO = LEE;
?WHO = KIM;
?WHO = ROBIN;
?WHO = SANDY;
?WHO = CATS;
?WHO = SANDY;
     
MUCH better!
           
(?- (likes robin lee))
[printout]
No.

Note that this does NOT mean that Robin
doesn't like Lee. It means that there is
no logical proof that Robin likes Lee 
given the facts and rules at hand.
                       
We can ask--which are the pairs that
like each other?
     
(?- (likes ?x ?y) (likes ?y ?x))

[printouts]

?Y = KIM
?X = SANDY;
?Y = SANDY
?X = SANDY;
?Y = SANDY
?X = SANDY;
?Y = SANDY
?X = KIM;
?Y = SANDY
?X = SANDY;
?Y = ?X1037
?X = ?X1037;
   
The last pair inducates that everyone likes
himself.

Now, one of the beauties of Prolog is that
it can handle infinite solution sets by
returning results one at a time. Our 
program doesn't do that--yet.
     
|#

#| Exercise 11.1
The representations of relations has been
a list whose first element is a symbol. 
However, for relations with no arguments,
some people prefer to write (<- p q r)
rather than (<- (p) (q) (r)). Make 
changes so that either form is acceptable.
|#

;; This can be done with the function
;; ensure-list, which is called in the 
;; first line of add-clause.

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

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

#| Exercise 11.2
Some people find the <- notation difficult
to read. Define macros RULE and FACT 
so that we can write

(fact (likes robin cats))
(rule (likes Sandy ?x) if (likes ?x cats)) |#
                                            
;; I will assume "if" is the only atomic
;; keyword that is added.

(defmacro fact (&rest clause)
  `(<- ,@clause))

(defmacro rule (&rest clause)
  `(<- ,@(remove 'if clause)))

;; Now we revamp all this so that
;; answers are provided one at a time.
;; This is done by changing our functions,
;; rather than employing pipes, which 
;; would be easier.

(defun prove-all (goals bindings) ; 2
  "Find a solution to the conjugation of
  goals."
  (cond
    ((eq bindings +fail+) +fail+)
    ((null goals) bindings)
    (t
      (prove 
        (first goals) 
        bindings 
        (rest goals)))))

(defun prove%% (goal bindings other-goals) ; 2
  "Return a list of possible solutions to
  goal."
  (some
    #'(lambda (clause)
        (let ((new-clause 
                (rename-variables clause)))
          (prove-all
            (append 
              (clause-body new-clause)
              other-goals)
            (unify
              goal
              (clause-head new-clause)
              bindings))))
      (get-clauses (predicate goal))))

;; To enable the continuation of a search
;; after a result has been returned, we
;; need to signal failure unconditionally.
;; this requires a primitive "clause", 
;; which will actually be a function.

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions
  to goal."
  (let ((clauses 
          (get-clauses 
            (predicate goal))))
    (if (listp clauses)
        (some
          #'(lambda (clause)
              (let ((new-clause
                      (rename-variables
                        clause)))
                (prove-all
                  (append
                    (clause-body new-clause)
                    other-goals)
                  (unify
                    goal
                    (clause-head new-clause)
                    bindings))))
            clauses)
        ;; The predicate's "clauses" can be
        ;; an atom: a primitive function
        ;; to call.
        (funcall 
          clauses 
          (rest goal)
          bindings
          other-goals))))

(defun top-level-prove (goals)
  (prove-all
    `(,@goals
      (show-prolog-vars 
        ,@(variables-in goals)))
    +no-bindings+)
  (format t "~&No.")
  (values))

(defun show-prolog-vars 
       (vars bindings other-goals)
  "Print each variable with its values.
  Then ask the user if more solutions
  are desired."
  (if (null vars)
      (format t "~&Yes")
      (let ((*standard-output*
             (make-broadcast-stream
              *query-io*
              *standard-output*)))
        (dolist (var vars)
          (format 
            t 
            "~&~A = ~A"
            var
            (subst-bindings bindings var)))))
  (if (continuep)
      +fail+
      (prove-all other-goals bindings)))

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

#|

p=> means printout

(<- (member ?item (?item . ?rest)))
=> MEMBER
(<- (member ?item (?x . ?rest))
    (member ?item ?rest))
=> MEMBER
   
(?- (member 2 ?list))
p=> ?LIST = (2 . ?REST1757)
;
p=> ?LIST = (?X1758 2 . ?REST1762)
;
p=> ?LIST = (?X1758 ?X1763 2 . ?REST1767)
;
p=> ?LIST = (?X1758 ?X1763 ?X1768 2 . ?REST1772)
.
p=> No.
    
Now it works with infinite results.
The query: which lists is 2 a member of?
All non-2 elements are variables.
                                
(<- (length () 0))
=> LENGTH
(<- (length (?x . ?y) (1+ ?n))
    (length ?y ?n))
=> LENGTH
   
(?- (length (a b c d) ?n))
p=> ?N = (1+ (1+ (1+ (1+ 0))))
;
p=> No.
    
(?- (length ?list (1+ (1+ 0))))
p=> ?LIST = (?X1828 ?X1831))
;
=> No.
     
(?- (length ?list ?n))
p=> ?LIST = NIL
p=> ?N = 0
;
p=> ?LIST = (?X1837)
p=> ?N = (1+ 0)
;
p=> ?LIST = (?X1837 ?X1840)
p=> ?N = (1+ (1+ 0))
;
p=> ?LIST = (?X1837 ?X1840 ?X1843)
p=> ?N = (1+ (1+ (1+ 0)))
.
p=> No.
      
But:

(?- (length ?l (1+ (1+ 0))) (member a ?l))
p=> ?L = (A ?X1849)
;
p=> ?L = (?X1854 A)
;
p=> No.
    
(?- (member a ?l) (length ?l (1+ 1+ 0)))
p=> ?L = (A ?X1849)
;
p=> ?L = (?X1854 A)
;
[Stack overflow or infinite loop]
       
This is because in the first query the first
clause constrains the second. But in the 
second query, the 1st clause is not 
constrained by the second, and an infinits,
fruitless search is begun once the only
two possible answers are returned.
    
This is a problem with Prolog, not our
implementation.

|#
 
#| We wish to add a general-purpose, 
anonymous variable to our implementation.
It can be used several times in a clause
without referring to the same thing. We 
the symbol ? to represent this variable.
    
|#
 
(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
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



#| The Zebra Puzzle
   
1. There are five houses in a line, each 
   with an owner, a pet, a cigarette, 
   a drink, and a color.
   
2. The Englishman lives in the red house.
   
3. The Spaniard owns the dog.
   
4. Coffee is drunk in the green house.
   
5. The Ukrainian drinks tea.
   
6. The green house is immediately to the
   right of the ivory house.
   
7. The Winston smoker owns snails.
 
8. Kools are smoked in the yellow house.
   
9. Milk is drunk in the middle house.
   
10. The Norwegian lives in the first house
    on the left.
    
11. The man who smokes Chesterfields lives
    next to the man with the fox.
    
12. Kools are smoked in the house next to 
    the house with the horse.
    
13. The Lucky Strike smoker drinks orange
    juice.
    
14. The Japanese man smokes Parliaments.
    
15. The Norwegian lives next to the blue
    house.
    
Question 1: Who drinks water?
Question 2: Who owns the zebra?
         
|#

(defun define-basic-relations ()
  (<- (member ?item (?item . ?rest)))
  (<- (member ?item (?x . ?rest))
      (member ?item ?rest))
  (<- (nextto ?x ?y ?list)
      (lright ?x ?y ?list))
  (<- (nextto ?x ?y ?list)
      (lright ?y ?x ?list))
  (<- (lright ?left ?right 
              (?left ?right . ?rest)))
  (<- (lright ?left ?right (?x . ?rest))
      (lright ?left ?right ?rest))
  (<- (= ?x ?x)))

(defun add-zebra-relations ()
  (define-basic-relations)
  (<- (zebra ?h ?w ?z)
      ;; Each house is of the form
      ;; (house nationality pet 
      ;;  cigarette drink house-color)
      (= ?h ((house norwegian ? ? ? ?) ; 1, 10
             ?
             (house ? ? ? milk ?) ? ?)) ; 9
      (member (house englishman ? ? ? red)
              ?h) ; 2
      (member (house spaniard dog ? ? ?)
              ?h) ; 3
      (member (house ? ? ? coffee green)
              ?h) ; 4
      (member (house ukrainian ? ? tea ?)
              ?h) ; 5
      (iright (house ? ? ? ? ivory)
              (house ? ? ? ? green)
              ?h) ; 6
      (member (house ? snails winston ? ?)
              ?h) ; 7
      (member (house ? ? kools ? yellow)
              ?h) ; 8
      (nextto (house ? ? chesterfield ? ?)
              (house ? fox ? ? ?) ; 11
              ?h)
      (nextto (house ? ? kools ? ?) ; 12
              (house ? horse ? ? ?)
              ?h)
      (member (house ? ? luckystrike ; 13
               orange-juice ?)
              ?h)
      (member (house japanese ? ; 14
               parliaments ? ?)
              ?h)
      (nextto (house norwegian ? ? ? ?)
              (house ? ? ? ? blue) ; 15
              ?h)
      ;; now for the questions
      (member (house ?w ? ? water ?) ?h)
      (member (house ?z zebra ? ? ?) ?h)))
      
             





    


             
      
      
        



                


     
     
       
       


