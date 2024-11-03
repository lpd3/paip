;;;; paip/ch-10-more-efficiency.lisp

;;;; Chapter 10: Low-Level Efficiency 
;;;; Issues
          
(in-package :more-efficiency)

;;; If you have the most efficient
;;; algorithm, employ delays, 
;;; memoize your functions, use 
;;; indexing and compile mini-languages,
;;; you could still have efficiency 
;;; issues. There are some lower-level
;;; techniques that can improve 
;;; efficiency up to a factor of 40.

;;; First, use declarations
                          
(defun square (x)
  "Given a number, return its square."
  (* x x))

(defun sum-squares (seq)
  "Given a sequence of numbers, 
  return the sum of their squares. 
  No declarations."
  (let ((sum 0))
    (dotimes (i (length seq))
      (incf sum (square (elt seq i))))
    sum))
    
(defun sum-squares-vec-of-ints (vect)
  "Given a vector of integers
  that are no bigger than the 
  word length of the platform, return
  the sum of their squares. Uses 
  declarations to improve speed."
  (declare 
    (type (simple-array fixnum *) vect)
    (inline square)
    (optimize speed (safety 0)))
  (let ((sum 0))
    (declare (fixnum sum))
    (dotimes (i (length vect))
      (declare (fixnum i))
      (incf sum
        (the fixnum 
             (square (svref vect i)))))))

#| explanation
   
(type (simple-array fixnum *) vect)
      
Specifies that the arg will be a 
simple-array of fixnums, integers
that fit in a system word. This
obviates the need to check the sequence
type of the arg and allows the compiler to
assume that regular integer arithmetic 
is to be expected.

(inline square)

On implementations that permit it, the
code of the square function will be 
written directly into the main call. This
gets rid of separate function calls for
square. The technique should be used for
small functions whose main overhead is 
the call itself.
 
(optimize speed (safety 0))

The compiler should make every effort to
make this function as fast as possible, 
and should sacrifice type checks and other
similar safety measures.
        
(declare (fixnum sum))
(declare (fixnum i))

Both values can be expected to operate 
under simple arithmetic rules.
      
(the fixnum ...
    
The intermediate square values will also
fit in a computer word, and can partake
of simple arithmetic.
   
Finum and Float (or better, a specific 
type of float) are the numeric type 
declarations that really matter.
         
Other important type declarations:
      
list and array
simple-vector, simple-array
(array type)
       
[use (simple-vector size) rather than
 (simple-vector type)]
                     
(array * dimensions)

|# 
  
#| 
  
Avoid Generic Functions.
      
When efficiency is of the utmost concern, 
use more specific functions: first/nth, 
svref, aref, char, bit instead of elt, for
example.
  
|#

#| 
  
Avoid complex argument lists
      
The longer the argument list, the more 
overhead one incurs. With &rest args, 
there is usually less overhead than with 
the same number of required args. Optional
and keyword args incur the biggest 
overhead penalty.
         
A good guideline: use kwargs as an 
interface to infrequently used functions,
meanwhile, maintain alternative versions
of the same functions that have no kwargs
   
|#

;; Example of an efficient use of kwargs

(proclaim '(inline key))

(defun key (&key a b (c 1) (d (sqrt a)))
  (%no-key a b c d))

(defun %no-key (a b c d)
  (list a b c d))

;; Another option is to create a macro
;; that does this sort of thing
                              
(defun ensure-atom (obj)
  (if (atom obj)
      obj
      (ensure-atom (first obj))))

(defmacro defun* 
          (fn-name arg-list &rest body)
  "Define two functions, one an interface
  to a &keyword-less version. Proclaim
  the interface function inline."
  (if (and (member '&key arg-list)
           (not (member '&rest arg-list)))
      (let ((no-key-fn-name
             (concat-symbol 
               fn-name 
               '*no-key))
            (args
             (mapcar #'ensure-atom
               (set-difference
                arg-list
                lambda-list-keywords))))
        `(progn 
           (proclaim '(inline ,fn-name))
           (defun ,no-key-fn-name ,args
             ,@body)
           (defun ,fn-name ,arg-list
             (,no-key-fn-name ,@args))))
      `(defun ,fn-name ,arg-list
         ,@body)))

;; Remember: these are suggestions for
;; sqeezing extra efficiency out of a 
;; program. Where efficiency is not 
;; crucial, and particularly in the 
;; development stage, kwargs are very, 
;; very useful.

#|

Avoid Unnecessary Consing
      
Lisp guarantees that temporary storage will
be reallocated when no longer needed. However,
the efficiency of this garbage collection
varies from compiler to compiler. 
Moreover, the user has no direct control
over the process.
     
In time-critical production-ready systems,
we often cannot afford to have the program
pause temporarily to reallocate memory.
The biggest generator of garbage is 
consing.

In these situations, we avoid unnecessary
copying of objects, and prefer the destructive
versions of functions.
         
nreverse instead of reverse
delete instead of remove
nconc intead of append
etc.

Another option is to forego lists entirely
and use vectors.
    
|#

(defun flatten1 (tree)
  "Given a nested list, return a flat
  list of the atoms. Simple version, and
  expensive because of consing and repeated
  traversal."
  (cond
   ((null tree)
    nil)
   ((atom tree)
    (list tree))
   (t
    (append
     (flatten1 (car tree))
     (flatten1 (cdr tree))))))
     
(defun flatten2 (tree &optional accumulator)
  "Much more efficient flatten function"
  (cond
   ((null tree)
    accumulator)
   ((atom tree)
    (cons atom accumulator))
   (t
    (flatten2
      (car tree)
      (flatten2
       (cdr tree)
       accumulator)))))
       
;; here is a more efficient version of 
;; pat-match, from the tools chapter. Note
;; that this version does not support
;; segment variables, etc.
                         
(defun variablep (obj)
  (and (symbolp obj)
       (char= (char (symbol-name obj) 0)
              #\?)))

(let* ((vars (make-array 
               10 
               :fill-pointer 0
               :adjustable t))
       (vals (make-array
              10
              :fill-pointer 0
              :adjustable t))
       (success (cons vars vals))
       (fail nil))
  (defun efficient-pat-match (pattern input)
    "Match pattern against input"
    (setf (fill-pointer vars) 0
          (fill-pointer vals) 0)
    (pat-match-1 pattern input))
  
  (defun pat-match-1 (pattern input)
    (cond
     ((variablep pattern)
      (match-var pattern input))
     ((eql pattern input)
      success)
     ((and (consp pattern) (consp input))
      (and
       (pat-match-1 
         (first pattern) 
         (first input))
       (pat-match-1
         (rest pattern)
         (rest input))))
     (t fail)))
  
  (defun match-var (var input)
    "Match a single variable against input"
    (let ((i (position var vars)))
      (cond
       ((null i)
        (vector-push-extend var vars)
        (vector-push-extend input vals)
        success)
       ((equal input
               (aref vals i))
        success)
       (t fail)))))

;; An even more efficient version
;; uses simple vectors, and replaces
;; the vectors with larger ones when
;; necessary

(let* ((current-size 0)
       (max-size 1)
       (vars (make-array max-size))
       (vals (make-array max-size))
       (success (cons vars vals))
       (fail nil))
  (declare (simple-vector vars vals)
           (fixnum current-size max-size))
  (defun efficient-pat-match-2 (pattern input)
    (setf current-size 0)
    (pat-match-1-2 pattern input))
    
  (defun pat-match-1-2 (pattern input)
    ;; unchanged
    (cond
     ((variablep pattern)
      (match-var-2 pattern input))
     ((eql pattern input)
      success)
     ((and (consp pattern) (consp input))
      (and
       (pat-match-1-2 
         (first pattern) 
         (first input))
       (pat-match-1-2
         (rest pattern)
         (rest input))))
     (t fail)))
  
  (defun match-var-2 (var input)
    (let ((i (position var vars)))
      (cond
       ((null i)
        (when (= current-size max-size)
          (setf max-size (* 2 max-size)
                vars (replace 
                       (make-array max-size)
                       vars)
                vals (replace
                       (make-array max-size)
                       vals)
                success (cons vars vals)))
        (setf (aref vars current-size) var
              (aref vals current-size) input)
        (incf current size)
        success)
       ((equal input (aref vals i))
        success)
       (t fail)))))

;; In situations where you absolutely must
;; use lists, consider the following 
;; version of cons, which conses only
;; when necessary:

(proclaim '(inline reuse-cons))

(defun reuse-cons (x y xy)
  "Return (cons x y), or just xy
  when it is the same thing.
  Useful only when x and y are
  symbols, numbers of the same type
  or characters, and xy either
  contains one element or is a 
  dotted list."
  (if (and (eql x (car xy))
           (eql y (cdr xy)))
      xy
      (cons x y)))

;; A definition that uses reuse-cons

(defun remq (item list)
  "Like ``remove'', but uses eq and 
  only works on lists."
  (cond
   ((null list)
    nil)
   ((eq item (first list))
    (remq item (rest list)))
   (t
    (reuse-cons
     (first list)
     (remq item (rest list))
     list))))

;; For situations where there is no 
;; known cons cell at hand to give to
;; reuse-cons as the xy, we can use 
;; a function that caches cons cells

(defvar *uniq-cons-table*
  (make-hash-table :test #'eq))

(defun ucons (x y)
  "Return a cons so that
  (eq (ucons x y) (ucons x y)) is true."
  (let ((car-table
         (or (gethash x *uniq-cons-table*)
             (setf
               (gethash x *uniq-cons-table*)
               (make-hash-table :test #'eq)))))
    (or (gethash y car-table)
        (setf
         (gethash y car-table)
         (cons x y)))))

(defvar *uniq-atom-table*
  (make-hash-table :test #'equal))

(defun unique (exp)
  "Return a canonical expression that is
  EQUAL to exp arg, such that (equal x y)
  implies (eq (unique x) (unique y))."
  (typecase exp
    (symbol exp)
    (fixnum exp) 
    ;; Remove the previous in implementations
    ;; in which fixnums with the same 
    ;; value are not eq.
    (atom
     (or (gethash exp *uniq-atom-table*)
         (setf
          (gethash exp *uniq-atom-table*)
          exp)))
    (cons
     (unique-cons 
       (car exp)
       (cdr exp)))))

(defun unique-cons% (x y)  ; 1
  "Return a cons such that
  (when (and (equal x x2)
             (equal y y2))
   -> (eq (unique-cons x y)
          (unique-cons x2 y2))"
   (ucons (unique x) (unique y)))

(defun ulist (&rest list)
  "Return a uniquified list of 
  the args."
  (unique args))

(defun uappend (x y)
  "Given two lists, return a uniquified
  list containing all the elements of 
  the first arg in order, followed
  by all the elements of the second 
  arg in order."
  (if (null x)
      (unique y)
      (ucons 
        (first x) 
        (uappend (rest x) y))))
        
;; This is not quite right. We need
;; to change uniqie-cons so that 
;; it handles trees properly, that is
;; so that it uniquifies any 
;; elements that are not already unique.
     
(defun unique-cons (x y) ; 2
  "Return a cons such that
  (when (and (equal x x2)
             (equal y y2))
   -> (eq (unique-cons x y)
          (unique-cons x2 y2))"
  (let (ux uy) ; unique x and y
    (let ((car-table
           (or (gethash x *uniq-cons-table*)
               (gethash
                 (setf ux (unique x))
                 *uniq-cons-table*)
               (setf
                 (gethash 
                   ux 
                   *uniq-cons-table*)
                 (make-hash-table 
                   :test #'eql)))))
      (or (gethash y car-table)
          (gethash 
            (setf uy (unique y))
            car-table)
          (setf
            (gethash uy car-table)
            (cons ux uy))))))

;; another advantage of unique lists is 
;; that they can help with indexing. 
;; A hash table that stores them can 
;; have an eq test, which could lead
;; to a significant efficiency gain 
;; with large data sets.

;; Another way of avoiding consing 
;; is to prefer multiple values over
;; structures. 

;; Sometimes, you may want to take over
;; the management of resources, possibly
;; improving on the native garbage 
;; collection. This makes sense when
;; three conditions are met:
;; 1. Instances are frequently created,
;; and are needed only temporarliy. 2. 
;; It is easy, or at the very least 
;; possible, to determine when the instances
;; are no longer needed, and thus can 
;; be recycled. 3. The instantiation
;; is costly.

;; It is quite likely that, even when the
;; three conditions are met, efforts to 
;; improve on garbage collection will not
;; make the program more efficient. However
;; the following macro may help in some
;; cases.

(defmacro defresource 
          (name
           &key
           constructor
           (initial-copies 0)
           (size (max initial-copies 10)))
  (let ((resource 
          (concat-symbol name '-resource))
        (deallocate
          (concat-symbol 'deallocate- name))
        (allocate
          (concat-symbol 'allocate- name)))
    `(let ((,resource
              (make-array ,size
                :fill-pointer 0)))
       (defun ,allocate ()
         "Get an element from the resource
         pool, or make one."
         (if (zerop (fill-pointer ,resource))
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer needed element
         back in the pool."
         (vector-push-extend 
           ,name 
           ,resource))
       ,(if (plusp initial-copies)
            `(mapc 
               #',deallocate
               (loop
                 repeat ,initial-copies
                 collect (,allocate))))
       ',name)))

;; an example that employs defresource

#|

(defresource buffer 
             :constructor (make-buffer)
             :size 100
             :initial-copies 10)
             
|#

;; We need to make sure that the allocated
;; instances no longer have active pointers
;; pointing to them when they are 
;; deallocated. The following context
;; manager can help prevent this

(defmacro with-resource
          ((var resource &optional protect)
           &body body)
  "Execute body with var bound to an
   instance of resource."
  (let ((allocate
          (concat-symbol 
            'allocate- 
            resource))
        (deallocate
          (concat-symbol
            'deallocate-
            resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect
             (progn
               (setf ,var (,allocate))
               ,@body)
             (unless (null ,var)
               (,deallocate ,var))))
         `(let ((,var (,allocate)))
            ,@body
            (,deallocate ,var)))))

;; A simpler alternative to pools, when 
;; possible, 
;; is to use a single instance that is 
;; recycled.

;; We could also choose safety over 
;; efficiency by adding type-checks to
;; the deallocate functions.

#| Using the Right Data Structure |#
   
;; Example 1: checking for and creating
;; variables. This is what we have been
;; doing in our pattern matching.

;; 1. Our choice was to define a variable
;; as a symbol beginning with ?

;; Other possible definitions of a variable

;; 2. All keywords
;; 3. A dedicated structure type 
;; 4. A read macro (say ?) that 
;;    returns a list whose first element
;;    is :var. There would be retranslation
;;    code.
;; 5. Negative integers (where these don't
;;    have meaning in the input)
;; etc., etc. 

;; Of the above approaches, the integers
;; will be fastest and the structures 
;; slowest. Keywords are faster than
;; the system we have been using.

;; Know what features are most efficient
;; on your system, and use these 
;; in critical portions of the program, 
;; using more straightforward approaches
;; elsewhere.

;; Example 2: lists

;; It is tempting to use lists for 
;; everything, since they are ubiquitous
;; in CL, with tons of built-in functions.
;; But often, a different structure will
;; be more efficient.

;; Vectors take up about half the space
;; in memory as lists. Moreover, they 
;; have low-cost random access. 
;; If your sequence must change size, 
;; adjustable vectors are more efficient
;; than lists.
;; Hash tables are more efficient 
;; than association lists when there 
;; are more than about 10 entries.

;; Case 3: Queues

;; Stacks are often implemented as 
;; plain lists. This is apt and very
;; efficient. Using a plain list
;; to implement a queue, on the other 
;; hand, is very inefficient. 
;; There are much better ways to
;; implement a queue.

;; One implementation is a dotted list with 
;; two elements. The car is a pointer
;; to the start of the queue and the cdr
;; is a pointer to the end. Here is 
;; the definition of tconc, which added
;; elements to queues in some obsolete
;; Lisp dialects:

(defun tconc (item q) ; 
  "Insert item at the end of a queue."
  (setf (cdr q)
        (if (null (cdr q))
            (setf (car q)
                  (cons item nil))
            (setf (cddr q)
                  (cons item nil)))))
                  
;; An improved implementation obviates
;; the need for branching on each 
;; insertion by a clever trick. In this
;; queue, the car points to the tail
;; of the contents and the cdr to the head.

(proclaim '(inline 
             queue-contents
             make-queue
             enqueue
             dequeue
             front
             empty-queue-p
             queue-nconc))

(defun queue-contents (q)
  (cdr q))

(defun make-queue ()
  "Build and return a new, empty queue."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert an item at the end of a queue."
  (setf (car q)
        (setf (cdar q)
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of a queue."
  (pop (cdr q))
  (unless (cdr q)
    (setf (car q) q))
  q)

(defun front (q)
  (car (queue-contents q)))

(defun empty-queue-p (q)
  (not (queue-contents q)))

(defun queue-nconc (q list)
  "Given a queue and a list, add
  the elements in the list to the 
  end of the queue."
  (setf (car q)
        (last (setf (cdar q) list)))
  q)

;;; Example 5: Tables

;;; Several choices:
;;; 1. Association lists.
;;;      Simple
;;;      Most efficient for small tables
;;;      Can have multiple entries with the
;;;      same key.
;;;      Can be sorted
;;;      Lookup of key by value is just as
;;;      easy as lookup of value by key
;;;      can be manipulated like lists
;;;      visible by default
;;; 2. Hash tables
;;;      much more efficient than association
;;;      lists for medium or large 
;;;      tables.
;;; 3. Property lists
;;;      very very simple
;;;      keys must be symbols
;;; 4. Arrays
;;;      Keys must be non-negative integers
;;;      or sets of non-negative integers
;;;      Both the simplest and most 
;;;      efficient solution.
;;; 5. Tries. These must be implemented.

(defstruct trie (value nil) (arcs nil))

(defconstant +trie-deleted+ 'deleted)

(defun put-trie (key trie value)
  "Set the value of any key in trie."
  (setf (trie-value (find-trie key t trie))
        value))

(defun get-trie (key trie)
  "Return the value for a key in a tree, 
  and a second foundp value."
  (let* ((key-trie (find-trie key nil trie))
         (val (when key-trie
                    (key-value key-trie))))
    (if (or (not key-trie)
            (eq val +trie-deleted+))
        (values nil nil)
        (values val t))))

(defun delete-trie (key trie)
  "Remove a key from a trie."
  (put-trie key trie +trie-deleted+))

(defun find-trie (key extendp trie)
  "Find the trie-node for this key.
  If extendp is t, make a new node if 
  one does not exist."
  (cond
   ((null trie)
    nil)
   ((atom key)
    (follow-arc key extendp trie))
   (t
    (find-trie 
      (cdr key) 
      extendp
      (find-trie
       (car key)
       extendp
       (find-trie
        "."
        extendp
        trie))))))

(defun follow-arc (component extendp trie)
  "Find the trie node for this component
  of the key. If extendp is t, make a 
  new node if need be."
  (let ((arc 
          (assoc 
            component 
            (trie-arcs trie))))
    (cond
     (arc (cdr arc))
     ((not extendp)
      nil)
     (t
      (let ((new-trie (make-trie)))
        (push (cons component new-trie)
              (trie-arcs trie))
        new-trie)))))

#|

Exercise 10.1 [h] Define the macro deftable, 
such that (deftable person assoc) will act 
much like a defstruct—it will define a set 
of functions for manipulating a table of 
people: get-person, put-person, 
clear-person, and map-person. The table 
should be implemented as an association 
list. Later on, you can change the 
representation of the table simply by 
changing the form to (deftable person hash),
person hash ), without having to change 
anything else in your code. Other 
implementation options include property 
lists and vectors. deftable should also 
take three keyword arguments: inline, size 
and test. Here is a possible macroexpansion:

> (macroexpand 
    '(deftable 
       person 
       hash 
       :inline t 
       :size 100)) ≡
       
(progn
  (proclaim 
    '(inline 
       get-person 
       put-person 
       map-person))
  (defparameter *person-table*
    (make-hash-table :test #eql :size 100))
  (defun get-person (x &optional default)
    (gethash x *person-table* default))
  (defun put-person (x value)
    (setf (gethash x *person-table*) value))
  (defun clear-person () 
    (clrhash *person-table*))
  (defun map-person (fn) 
    (maphash fn *person-table*))
  (defsetf get-person put-person)
  'person) 
          
|#   
    
(defmacro with-gensyms ((&rest syms)
                        &body body)
  `(let ,(mapcar
           #'(lambda (sym)
               `(,sym (gensym 
                        ,(symbol-name sym))))
             syms)
     ,@body))

(defmacro deftable
          (name 
           type 
           &key inline 
                (size 20) 
                (test #'eql))
    (let ((get-t (concat-symbol
                     'get- name))
           (put-t (concat-symbol
                     'put- name))
           (clear-t (concat-symbol
                       'clear- name))
           (map-t (concat-symbol
                     'map- name))
           (t-tab (concat-symbol
                     '* name '-table*)))
      `(progn
         ,(when inline
           `(declare '(inline
                      ,get-t
                      ,put-t
                      ,map-t)))
       ,(ccase type
          (assoc
            `(let ((,t-tab nil))
               (defun ,get-t (k)
                 (cdr
                  (assoc 
                    k 
                    ,t-tab 
                    :test ,test)))
               (defun ,put-t (k v)
                 (setf 
                   ,t-tab
                   (acons k v ,t-tab)))
               (defun ,clear-t ()
                 (setf ,t-tab nil))
               (defun ,map-t (fn)
                 (mapcar
                  #'(lambda (entry)
                      (let ((k (car entry))
                            (v (cdr entry)))
                        (funcall fn k v)))
                    ,t-tab))
               ',name))
          (hash 
            (with-gensyms (coll k v)
            `(let ((,t-tab
                    (make-hash-table
                     :test ,test
                     :size ,size)))
               (defun ,get-t (k)
                 (gethash k ,t-tab))
               (defun ,put-t (k v)
                 (setf
                  (gethash k ,t-tab)
                  v))
               (defun ,clear-t ()
                 (clrhash ,t-tab))
               (defun ,map-t (fn)
                 (let ((,coll nil))
                   (maphash
                     #'(lambda (k v)
                         (push 
                           (funcall fn k v)
                           ,coll))
                       ,t-tab)
                   ,coll))
               ',name)))
          (plist
            (when 
             (not 
               (member test
                  (list #'eq #'eql)
                  :test #'equal))
             (error
              "only EQ or EQL tests permitted with~%~
              plist tables"))
           ;; The table is the plist of
           ;; the symbol bound to t-tab
            (with-gensyms (ks vs rplist i rest k v)
              `(progn
                 (defun ,get-t (k)
                   (get ',t-tab k))
                 (defun ,put-t (k v)
                   (setf
                     (get ',t-tab k)
                     v))
                 (defun ,clear-t ()
                   (setf
                     (symbol-plist ',t-tab)
                     nil))
                 (defun ,map-t (fn)
                   (let ((,ks nil)
                         (,vs nil)
                         (,rplist 
                           (reverse
                             (symbol-plist
                               ',t-tab))))
                     (do ((,i 0 (1+ ,i))
                          (,rest 
                            ,rplist 
                            (cdr ,rest)))
                         ((null ,rest))
                       (if (evenp ,i)
                           (push 
                             (car ,rest) 
                             ,vs)
                           (push 
                             (car ,rest) 
                             ,ks)))
                       (mapcar
                        #'(lambda (,k ,v)
                            (funcall fn ,k ,v))
                         ,ks
                         ,vs)))
                 ',name)))
          (vector 
            (with-gensyms (ks vs i)
              `(let ((,ks
                       (make-array ,size
                         :adjustable t
                         :fill-pointer 0))
                     (,vs
                       (make-array ,size
                         :adjustable t
                         :fill-pointer 0)))
                 (defun ,get-t (k)
                   (let ((,i
                          (position k ,ks
                            :test ,test)))
                     (when ,i
                       (aref ,vs ,i))))
                 (defun ,put-t (k v)
                   (vector-push-extend
                    k ,ks)
                   (vector-push-extend
                    v ,vs))
                 (defun ,clear-t ()
                   (setf ,ks
                     (make-array ,size
                       :adjustable t
                       :fill-pointer 0))
                   (setf ,vs
                     (make-array ,size
                       :adjustable t
                       :fill-pointer 0))
                   t)
                 (defun ,map-t (fn)
                   (map 
                    'list 
                    fn 
                    ,ks 
                    ,vs))
                 ',name)))))))
                
           
             
               
               
                   
            
     

  
            
                  





        
        
  

                                 
  



            