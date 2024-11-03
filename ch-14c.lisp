;;;; paip/ch-14c
               
(in-package :reasoning-possible-worlds)

(defvar *world*% 'wo
  "The current world used by index and 
  fetch.")

(defun index (key &optional (world *world*))
  "Store key in a dtree node. Key must be
  (predicate. args); it is stored in the 
  dtree, indexed by the world."
  (dtree-index 
    key 
    key 
    world
    (get-dtree ; *
      (predicate ; *
        key))))

(defun dtree-index (key value world dtree)
  "Index value under all atoms of key in 
  dtree."
  (cond
   ((consp key) ; index on both first and rest
    (dtree-index 
      (first key) 
      value 
      world
      (or 
         (dtree-first ; *
           dtree)
         (setf (dtree-first dtree)
               (make-dtree)))) ; *
    (dtree-index
      (rest key)
      value
      world
      (or
         (dtree-rest ; *
           dtree)
         (setf (dtree-rest dtree)
               (make-dtree)))))
   ((null key)) ; don't index on nil
   ((variablep ; *
      key) ; index a variable
    (nalist-push
      world
      value
      (dtree-var ; *
        dtree)))
   (t ; Make sure there is an nlist for 
      ; this atom, and add to it
     (nalist-push 
       world
       value
       (lookup-atom ; *
         key dtree)))))

(defun nalist-push (key val nalist)
  "Index val under key in a numbered alist."
  ;; An nalist is of the form 
  ;; (count (key val*)*)
  (incf (car nalist))
  (let ((pair (assoc key (cdr nalist))))
    (if pair
        (push val (cdr pair))
        (push (list key val)
              (cdr nalist)))))

(defstruct (world (:print-function
                   print-world))
  name parents current)

(defun get-world (name 
                  &optional
                  current
                  (parents (list *world*)))
  "Look up or create the world with this 
  name. If the world is new, give it the
  list of parents."
  (cond
   ((world-p name)
    name)
   ((get name 'world))
   (t
    (setf (get name 'world)
          (make-world
            :name name
            :parents parents
            :current current)))))
            
(defvar *world* (get-world 'wo nil nil) ; 2
  "The current world used by index and 
  fetch.")
  
(defun use-world (world)
  "Make this world current."
  ;; If passed a name, look up the 
  ;; world it names.
  (setf world (get-world world))
  (unless (eq world *world*)
    ;; Turn the old world(s) off and the
    ;; new world(s) on, unless we are 
    ;; already using the new world
    (set-world-current *world* nil) ; *
    (set-world-current world t)
    (setf *world* world)))

(defun use-new-world ()
  "Make up a new world and use it. The
  new world inherits from the current
  world."
  (setf *world* (get-world (gensym "W")))
  (setf (world-current *world*) t)
  *world*)

(defun set-world-current (world on/off)
  "Set the 'current' field of world and
  its parents on or off."
  ;; NIL is off. Anything else is on.
  (setf (world-current world) on/off)
  (dolist (parent (world-parents world))
    (set-world-current parent on/off)))

(defun print-world (world 
                    &optional
                    (stream t)
                    depth)
  (declare (ignore depth))
  (prin1 (world-name world) stream))

(defun mapc-retrieve-in-world (fn query)
  "For every fact in the current world
  that matches the query, apply the fn
  to the binding list."
  (dolist (bucket (fetch query)) ; *
    (dolist (world/entries bucket)
      (when (world-current
             (first world/entries))
        (dolist (answer (rest world/entries))
          (let ((bindings 
                  (unify query answer))); *
            (unless (eq bindings +fail+) ; *
              (funcall fn bindings))))))))

(defun retrieve-in-world (query)
  "Find all facts that match query. 
  Return a list of bindings."
  (let ((answers nil))
    (mapc-retrieve-in-world
     #'(lambda (bindings)
         (push bindings answers))
       query)
    answers))

(defun retrieve-bagof-in-world (query)
  "Find all facts in current world that 
  match query. Return a list of queries
  with bindings filled in."
  (mapcar
   #'(lambda (bindings)
       (subst-bindings bindings query)) ; *
     (retrieve-in-world query)))
     
#| Repeat functions here from earlier
   namespaces because they rely on our
   refactoring |#
   
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
         
(defparameter *primitives*
   '(and sub ind rel val))


    


                                 
                        
               