;;;; paip/ch-14b
               
;;;; Continuation of chapter 14: making
;;;; the program more expressive

(in-package :reasoning-expressiveness)

#| Next, we handle the lack of expressiveness
in our language. The first part of this is 
to allow higher-order predicates. These
will include:
     
sub = subtype
(sub dog animal) 
a dog is a type of animal
                                           
rel = relation
(rel birthday animal date)
the birthday relation holds for each animal
and some date
  
ind = individual
(ind fido dog)
One dog is Fido
    
val = value
(val birthday fido july-1)
Fido's birthday is on July 1st
       
and
(and a b)
a and b 
|#
 
(defun add-fact% (fact) 
  "Add a fact to the database."
  (if (eq (predicate fact) 'and)
      (mapc #'add-fact (args fact))
      (index fact)))

(defun retrieve-fact (query 
                      &optional
                      (bindings +no-bindings+))
  "Find all facts that match query. Return
  a list of bindings."
  (if (eq (predicate query) 'and)
      (retrieve-conjunction (args query) (list bindings))
      (retrieve query bindings)))

(defun retrieve-conjunction (conjuncts
                             bindings-lists)
  "Return a list of binding-lists satisfying
  the conjuncts."
  (mapcan
    #'(lambda (bindings)
        (cond
          ((eq bindings +fail+)
           nil)
          ((null conjuncts)
           (list bindings))
          (t
           (retrieve-conjunction
             (rest conjuncts)
             (retrieve-fact
               (subst-bindings 
                 bindings
                 (first conjuncts))
               bindings)))))
      bindings-lists))

;; We must refactor the following two
;; functions

(defun mapc-retrieve (fn query 
                      &optional
                      (bindings +no-bindings+))
  "For every fact that matches the query, 
  apply the function to the binding list."
  (dolist (bucket (fetch query))
    (dolist (answer bucket)
      (let ((new-bindings 
              (unify query answer bindings)))
        (unless (eq new-bindings +fail+)
          (funcall fn new-bindings))))))

(defun retrieve (query &optional 
                 (bindings +no-bindings+))
  "Find all facts that match query. Return
  a list of bindings."
  (let ((answers nil))
    (mapc-retrieve 
      #'(lambda (bindings)
          (push bindings answers))
      query bindings)
    answers))

;; And we add these two operators, which 
;; depend
;; on the two previous functions

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
         
(defun args (x)
  (rest x))

#|

(add-fact '(sub dog animal)) => T
(add-fact '(sub bear animal)) => T
(add-fact '(ind fido dog)) => T
(add-fact '(ind yogi bear)) => T
(add-fact '(val color yogi brown)) => T
(add-fact '(val color fido golden)) => T
(add-fact '(val latin-name bear ursidae)) => T
(add-fact '(val latin-name dog canis-familiaris)) => T
        
(retrieve-fact '(sub ?kind animal))
=> (((?KIND . DOG))
    ((?KIND . BEAR)))
(retrieve-fact '(and (sub ?kind animal)
                     (val latin-name ?kind ?latin)))
=> (((?LATIN . CANIS-FAMILIARIS)
     (?KIND . DOG))
    ((?LATIN . URSIDAE)
     (?KIND . BEAR)))
   
|#

#| Now for some improvements. First, two
functions that return answers to queries
like retrieve-matches |#
  
(defun retrieve-bagof (query)
   "Find all facts that match query.  
   Return a list of queries with bindings
   filled in."
   (mapcar
     #'(lambda (bindings)
         (subst-bindings bindings query))
     (retrieve-fact query)))

(defun retrieve-setof (query)
  "Find all facts that match query. Return
  a list of unique queries with bindings
  filled in."
  >remove-duplicates 
    (retrieve-bagof query)
    :test #'equal)

;; Now we add error checking and the 
;; derivation of facts from the facts 
;; already entered. Unlike Prolog, which
;; uses a back-chaining approach, we 
;; sacrifice space for speed by creating
;; a forward-chaining version.

(defparameter *primitives*
   '(and sub ind rel val))

(defun add-fact (fact)
  "Add the fact to the data base."
  (cond
    ((eq (predicate fact) 'and)
     (mapc #'add-fact (args fact)))
    ((or
       (not (every #'atom fact))
       (some #'variablep (args fact))
       (not 
         (member 
           (predicate fact) 
           *primitives*)))
     (error "Ill-formed fact: ~A" fact))
    ((not (fact-present-p fact))
     (index fact)
     (run-attached-fn fact)))
  t)

(defun fact-present-p (fact)
  "Is this fact present in the data base?"
  (retrieve fact))

(defun run-attached-fn (fact)
  "Run the function associated with the 
  predicate of this fact."
  (apply
    (get (predicate fact) 'attached-fn)
    (args fact)))

(defmacro def-attached-fn (pred args 
                           &body body)
  "Define the attached function for a 
  primitive."
  `(setf (get ',pred 'attached-fn)
         #'(lambda ,args ,@body)))

(def-attached-fn ind (individual category)
  ;; Cache facts about inherited categories
  (query-bind (?super) `(sub ,category ?super)
    (add-fact `(ind ,individual ,?super))))

(def-attached-fn val (relation ind1 ind2)
  ;; Make sure the individuals are the right
  ;; kinds.
  (query-bind 
       (?cat1 ?cat2) 
       `(rel ,relation ?cat1 ?cat2)
    (add-fact `(ind ,ind1 ,?cat1))
    (add-fact `(ind ,ind2 ,?cat2))))

(def-attached-fn rel (relation cat1 cat2)
  ;; Run attached function for any IND's
  ;; of this relation
  (query-bind
       (?a ?b) `(ind ,relation ?a ?b)
    (run-attached-fn 
      `(ind ,relation ,?a ,?b))))
      
(def-attached-fn sub (subcat supercat)
  ;; Cache SUB facts
  (query-bind 
       (?super-super)
       `(sub ,supercat ?super-super) ;
    (index-new-fact
      `(sub ,subcat ,?super-super)) ;
    (query-bind 
      (?sub-sub)
            `(sub ?sub-sub ,subcat) ;
         (index-new-fact
           `(sub ,?sub-sub ,?super-super)))) ;
  (query-bind
       (?sub-sub)
       `(sub ?sub-sub ,subcat) ;
    (index-new-fact
      `(sub ,?sub-sub ,supercat))) ;
    ;; Cache IND facts
  (query-bind
       (?super-super)
       `(sub ,subcat ?super-super) ;
    (query-bind
         (?sub-sub)
         `(sub ?sub-sub ,supercat) ;
      (query-bind 
           (?ind)
           `(ind ?ind ,?sub-sub) ;
        (index-new-fact 
          `(ind ,?ind ,?super-super))))))
          
(defun index-new-fact (fact)
  "Index the fact in the data base unless
  it's already there."
  (unless (fact-present-p fact)
    (index fact)))

(defun test-bears ()
  (clear-dtrees)
  (mapc #'add-fact
        '((sub animal living-thing)
          (sub living-thing thing)
          (sub polar-bear bear)
          (sub grizzly bear)
          (ind yogi bear)
          (ind lars polar-bear)
          (ind helga grizzly)))
  (trace index)
  (add-fact '(sub bear animal))
  (untrace index))

#|

(test-bears)
[trace output]
"1> (INDEX (SUB BEAR ANIMAL))
<1 (INDEX T)
1> (INDEX (SUB BEAR THING))
<1 (INDEX T)
1> (INDEX (SUB GRIZZLY THING))
<1 (INDEX T)
1> (INDEX (SUB POLAR-BEAR THING))
<1 (INDEX T)
1> (INDEX (SUB BEAR LIVING-THING))
<1 (INDEX T)
1> (INDEX (SUB GRIZZLY LIVING-THING))
<1 (INDEX T)
1> (INDEX (SUB POLAR-BEAR LIVING-THING))
<1 (INDEX T)
1> (INDEX (SUB GRIZZLY ANIMAL))
<1 (INDEX T)
1> (INDEX (SUB POLAR-BEAR ANIMAL))
<1 (INDEX T)
1> (INDEX (IND LARS LIVING-THING))
<1 (INDEX T)
1> (INDEX (IND HELGA LIVING-THING))
<1 (INDEX T)
1> (INDEX (IND YOGI LIVING-THING))
<1 (INDEX T)
1> (INDEX (IND LARS THING))
<1 (INDEX T)
1> (INDEX (IND HELGA THING))
<1 (INDEX T)
1> (INDEX (IND YOGI THING))
<1 (INDEX T)
1> (INDEX (IND LARS ANIMAL))
<1 (INDEX T)
1> (INDEX (IND HELGA ANIMAL))
<1 (INDEX T)
1> (INDEX (IND YOGI ANIMAL))
<1 (INDEX T)
"    
|#

#| Now, we will refactor the code to 
supply a more concise, clearer syntax.
This is called a frame syntax. 
|#

(defmacro a (&rest args)
  "Define a new individual, and assert
  facts about it, in the database."
  `(add-fact 
     ',(translate-exp (cons 'a args))))

(defmacro each (&rest args)
  "Define a new category, and assert facts 
  about it in the database."
  `(add-fact 
     ',(translate-exp (cons 'each args))))

(defmacro ?? (&rest queries)
  "Return a list of answers satisfying 
  the query or queries."
  `(retrieve-setof
     ',(translate-exp
         (maybe-add
           'and
           (replace-?-vars queries))
         :query)))

(defun translate-exp (exp 
                      &optional query-mode-p)
  "Translate expression into a conjunction
  of the four primitives."
  (let ((conjuncts nil))
    (labels
      ((collect-fact (&rest terms)
         (push terms conjuncts))
       (translate (exp)
         ;; figure out what kind of 
         ;; expression this is
         (cond
          ((atom exp) exp)
          ((eq (first exp) 'a)
           (translate-a (rest exp)))
          ((eq (first ecp) 'each)
           (translate-each (rest exp)))
          (t
           (apply #'collect-fact exp)
           exp)))
       (translate-a (args)
         ;; translate (A category (rep filler))
         (let* ((category (pop args))
                (self
                 (cond
                  ((and args
                        (atom (first args)))
                   (pop args))
                  (query-mode-p
                   (gentemp "?"))
                  (t 
                    (gentemp
                      (string category))))))
           (collect-fact 'ind self category)
           (dolist (slot args)
             (translate-slot
               'val
               self
               slot))
           self))
       (translate-each (args)
         ;; translate 
         ;; (EACH category 
         ;;   [(isa *cat)]
         ;;   (slot cat*))
         (let ((category (pop args)))
           (when 
             (eq 
                (predicate 
                  (first args))
                'isa)
             (dolist (super (rest (pop args)))
               (collect-fact 
                 'sub 
                 category 
                 super)))
           (dolist (slot args)
             (translate-slot 
              'rel 
              category 
              slot))
           category))
       (translate-slot (primitive self slot)
         ;; translate (relation value)
         ;; into a REL or SUB
         (assert (= (length slot) 2))
         (collect-fact
          primitive
          (first slot)
          self
          (translate
           (second slot)))))
      ;; body of translate-exp
      (translate exp)
      ;; build up the list of conjuncts
      (maybe-add 
        'and
        (nreverse conjuncts)))))
        
(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t)
  returns t if exps is nil, (first exps)
  if there is only one and (and ,@exps if
  there are more than one."
  (cond
   ((null exps)
    if-nil)
   ((length=1 exps)
    (first exps))
   (t
    (cons op exps))))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (= (length x) 1)))

(defun replace-?-vars (exp)
  "Replace each ? var with a temporary var:
  ?123."
  (cond
   ((eq exp '?)
    (gentemp "?"))
   ((atom exp)
    exp)
   (t
    (reuse-cons
     (replace-?-vars (first exp))
     (replace-?-vars (rest exp))
     exp))))
     
#| We have a few capabilities to add: 1.
distinguishing unknown from false. 2. 
representing negations 3. representing
disjunctions. 4. representing multiple 
states of affairs. These can be provided 
by adding two things: possible worlds and
negated predicates. Since the added solutions
require refactoring our program, we 
continue in the next file with a new
namespace |#