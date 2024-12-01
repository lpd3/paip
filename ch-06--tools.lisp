;;;; paip/ch-6--tools.lisp

;;;; Chapter 6: Building Software Tools
                                      
(in-package :tools) 
            
(defparameter ?[ '?[)

(defparameter ?] '?])


(defun interactive-interpreter
    (prompt transformer
     &key (safe t)
          (quit '("q" "quit" "exit"))
          (stream *query-io*))
    "interactive-interpreter
       (prompt transformer &key safe
        quit stream) -> 'DONE
     prompt: string or function
     transformer: function
     safe: generalized Boolean. default t.
     quit: string or list of strings.
           default: (``q'' ``quit'' ``exit'')
     stream: stream object. default: *query-io*.
     
    Interactive program loop. On each
    loop, if prompt is a string, it is
    printed to stream. Otherwise, it must
    be a function of no args that 
    will be called at that time. 
    A line from the user is read
    into a string. If the string contains
    the quit string, or one of the quit 
    strings, the loop is exited and DONE
    is returned. Otherwise, the first
    form is read from the string--with safe
    io syntax if safe is t--and the result
    of invoking transformer on form
    is printed to stream. The rest of the
    line is discarded. If an error occurs, 
    the error message is printed to stream,
    and the loop continues."
  (let ((quit (ensure-list quit))
        (*standard-output* stream))
    (loop
      (handler-case
        (progn
          (if (stringp prompt)
              (print prompt)
              (funcall prompt))
          (let* ((line (read-line))
                 (split 
                   (uiop:split-string line)))
            (dolist (q quit)
              (when 
                (member q split :test #'equalp)
                (return-from
                 interactive-interpreter
                 'done)))
            (print
             (funcall
              transformer
              (if safe
                  (uiop:safe-read-from-string
                   line)
                  (read-from-string line))))))
        (error (condition)
          (format t 
            "~&;; Error~%~A~%Ignored."
            condition))))))

(defun prompt-generator (&optional
                         (num 0)
                         (control "[~D] "))
  "Return a function that prints prompts
  like [1], [2], etc."
  (lambda ()
    (format t control (incf num))))

;; A general pattern matcher follows.
;; The pattern matching language is now
;; detailed.

;; pat -> var, constant, segment-pat, 
;;        single-pat, (pat . pat)
                                
;; var -> a symbol starting with ?
;;        e.g. ?chars

;; constant -> any symbol that is not a 
;;             a Lisp keyword or variable.
;;             e.g. hello

;; segment-pat -> (:* <pat>), (:+ <pat>),
;;                (:? <pat>), (:if <pat>)
                                         
;; single-pat -> :is <var> <pred>,
;;               :or <pat1> <pat2> ...
;;               :and <pat1> <pat2> ...
;;               :not <pat1> <pat2> ...

;; (pat1 . pat2) -> pat1 matches the 
;;                  beginning, pat2
;;                  matches the rest

;; (:* <pat>)      -> pat matches zero or 
;;                  more tokens

;; (:+ <pat>)      -> pat matches one or more
;;                  tokens

;; (:? <pat>)      -> pat matches 0 or 1
;;                  tokens

;; (:if <expr>)    -> test if expr, which 
;;                    may contain vars, is
;;                    true

;; :is <var> <pred> -> test predicate on
;;                     one token

;; :or <pat1> <pat2> ... -> test each pat
;;                          on token, 
;;                         matching if any pat matches

;; :and <pat1> <pat2> ... -> test each pat
;;                           on token, matching only if all pats match

;; :not <pat1> <pat2> ... -> match if none of pats do

(defconstant +fail+ nil
  "Indicates pat-match failure.")

(defconstant +no-bindings+ '((t . t))
  "Indicates pat-match success, with no
  variables.")

(defun variablep (x)
  "Is x a variable, i.e., a symbol 
  beginning with ``?'' ?"
  (and (symbolp x)
       (eql
        (char
         (symbol-name x)
         0)
        #\?)))

(defun get-binding (var bindings)
  "Find a (variable . binding) pair 
  in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single
  binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single
  binding."
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a
  binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (variable . value) pair to a 
  binding list."
  (cons (make-binding var val)
        ;; Once we get rid of the 
        ;; dummy no bindings
        (if (tree-equal bindings +no-bindings+)
            nil
            bindings)))

(defun match-variable (var input bindings)
  "Does var match input? Uses (or updates)
  and returns bindings."
  (let ((binding (get-binding var bindings)))
    (if binding
        (if
         (equal
          input
          (binding-val binding))
         bindings
         +fail+)
        (extend-bindings var input bindings))))

(defun pat-match 
       (pattern input 
        &optional (bindings +no-bindings+)
                  (var? #'variablep))
  "Match pattern against input in the 
  context of the bindings."
  (cond
   ((eq bindings +fail+)
    +fail+)
   ((funcall var? pattern)
    (match-variable pattern input bindings))
   ((eql pattern input) bindings)
   ((and (consp pattern)
         (eq (first pattern) ?[))
    (multiple-value-bind
        (rest-pattern rest-input new-bindings)
        (literal-match 
          (rest pattern)
          input
          bindings)
      (cond
        ((eq new-bindings +fail+)
         +fail+)
        ((and (null rest-pattern)
              (null rest-input))
         new-bindings)
        ((or (null rest-pattern)
             (null rest-input))
         +fail+)
        (t
         (pat-match
          rest-pattern
          rest-input
          new-bindings
          var?)))))
   ((segment-pattern-p pattern)
    (segment-matcher pattern input bindings var?))
   ((single-pattern-p pattern)
    (single-matcher pattern input bindings var?))
   ((and (consp pattern) (consp input))
    (pat-match
     (rest pattern)
     (rest input)
     (pat-match
      (first pattern)
      (first input)
      bindings
      var?)
     var?))
   (t +fail+)))

;; implementing various single- and 
;; segment-pat symbols via a table 
;; implemented with property lists.

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 
      'segment-match+)
(setf (get '?? 'segment-match) 
      'segment-match?)
(setf (get '?if 'segment-match)
      'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern, 
  like ((:* var) . pat) ?"
  (and (consp pattern)
       (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn 
         (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g., (:is x predicate) (:and . patterns)
  (:or . patterns)"
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings var?)
  "Call the right function for this type
  of segment pattern."
  (funcall (segment-match-fn
            (first (first pattern)))
            pattern
            input
            bindings
            var?))

(defun single-matcher (pattern input bindings var?)
  "Call the right function for this type 
  of single pattern."
  (funcall
   (single-match-fn
    (first pattern))
   (rest pattern)
   input
   bindings
   var?))

(defun segment-match-fn (x)
  "Get the segment match function for x,
  if it is a symbol that has one."
  (when (symbolp x)
    (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single match function for x,
  if it is a symbol that has one."
  (when (symbolp x)
    (get x 'single-match)))

(defun match-is (var-and-pred input bindings var?)
  "Succeed and bind var if the input 
  satisfies pred, where var-and-pred is 
  the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings 
           (pat-match var input bindings var?)))
    (if (or (eq new-bindings +fail+)
            (not (funcall pred input)))
        +fail+
        new-bindings)))

(defun match-and (patterns input bindings var?)
  "Succeed if all the patterns match the
  input."
  (cond
   ((eq bindings +fail+)
    +fail+)
   ((null patterns)
    bindings)
   (t
    (match-and 
      (rest patterns)
      input
      (pat-match
       (first patterns)
       input
       bindings
       var?)
      var?))))

(defun match-or (patterns input bindings var?)
  "Succeed if any one of the patterns 
  matches the input."
  (if (null patterns)
      +fail+
      (let ((new-bindings
             (pat-match 
               (first patterns)
               input
               bindings
               var?)))
        (if (eq new-bindings +fail+)
            (match-or 
              (rest patterns)
              input
              bindings
              var?)
            new-bindings))))

(defun segment-match (pattern input bindings var?
                      &optional (start 0))
  "Match the segment pattern ((:* var) . pat)
  against the input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos 
                (first-match-pos
                 (first pat)
                 input
                 start
                 var?)))
          (if (null pos)
              +fail+
              (let ((b2 (pat-match
                         pat
                         (subseq input pos)
                         (match-variable
                          var
                          (subseq 
                            input
                            0
                            pos)
                          bindings)
                         var?)))
                ;; If this match failed, try
                ;; another one
                (if (eq b2 +fail+)
                    (segment-match
                      pattern
                      input
                      bindings
                      var?
                      (1+ pos))
                    b2)))))))

(defun first-match-pos (pat1 input start var?)
  "Find the first position that pat1 could
  possibly match input, starting at 
  position start. If pat1 is non-constant,
  then just return start."
  (cond
   ((and (atom pat1) 
         (not (funcall var? pat1)))
    (position 
      pat1 
      input 
      :start start
      :test #'equal))
   ((< start (length input)) start)
   (t nil)))
   


#|
(pat-match '(a (:* ?x) d) '(a b c d))
-> ((?X B C))
   
(pat-match '(a (:* ?x) (:* ?y) d) '(a b c d))
-> ((?Y B C) (?X))
   
[?x is bound to nothing....which is fine.
When a var follows a segment var, a match
is first attempted with the smallest length
match for the segment var. For :* vars, 
this is 0 elements. Since we can match with
?x having zero elts and ?y the rest, that 
is returned.]
   
(pat-match '(a (:* ?x) (:* ?y) ?x ?y)
           '(a b c d (b c) (d))) 
-> ((?Y D) (?X B C))
   
[Here, an ?x with zero elts is tried first.
No match. Then a ?x with one elt: B. Again,
no match. Then a ?x with two elts: B C.
That works, with ?y bound to D.]
|#

(defun segment-match+ (pattern input bindings var?)
  "Match one or more elements of input."
  (segment-match pattern input bindings var? 1))

(defun segment-match? (pattern input bindings var?)
  "Match 0 or 1 elements of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or
     (pat-match (cons var pat) input bindings var?)
     (pat-match pat input bindings var?))))

(defun match-if (pattern input bindings var?)
  "Test an arbitrary Lisp expression
  using variables. The pattern looks like
  ((:if <code>) . <rest>)."
  (and
   (progv 
     (mapcar #'car bindings)
     (mapcar #'cdr bindings)
     (eval (second (first pattern))))
   (pat-match (rest pattern) input bindings var?)))

(defun literal-match (pattern input bindings)
  (cond
    ((eq bindings +fail+)
     (values
      pattern
      input
      +fail+))
    ((or (null pattern) 
         (null input))
     (values
      pattern
      input
      bindings))
    ((eq (first pattern) ?])
     (values
      (rest pattern)
      input
      bindings))
    ((and (consp (first pattern))
          (consp (first input)))
     (if (tree-equal (first pattern)
                     (first input)
                     :test #'equal)
         (literal-match (rest pattern)
                        (rest input)
                        bindings)
         (values
          pattern
          input
          +fail+)))
    ((equal (first pattern) (first input))
     (literal-match (rest pattern)
                    (rest input)
                    bindings))
    (t
     (values
      pattern
      input
      +fail+))))   
         
#|
 
(pat-match '(?x ?op ?y is ?x 
             (:if (eql (funcall ?op ?x ?y) ?z)))
           '(3 + 4 is 7))
-> ((Z . 7) (Y . 4) (OP . +) (X . 3))   
                                       
(pat-match '(?x ?op ?y 
              (:if (funcall ?op ?x ?y)))
           '(3 > 4))
-> NIL
   
|#

;; It would be nice if we could write
;; (a ?x* ?y* d) instead of
;; (a (:* ?x) (:* ?y) d). To make 
;; the syntax change for the program 
;; would mean much more code for the 
;; parsing. But we can have a translator
;; that allows the user to write the simpler
;; form and have it translated before
;; processing. This is relatively
;; painless.

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for 
  a pat-match pattern."
  (setf
   (get symbol 'expand-pat-match-abbrev)
   (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern-matching 
  abbreviations in pat."
  (let ((*escaped* nil))
    (declare (special *escaped*))
    (labels ((rec (subpat)
               (cond
                ((eql subpat ?[)
                 (setf *escaped* t))
                ((eql subpat ?])
                 (setf *escaped* nil))
                ((and 
                   (not *escaped*)
                   (symbolp subpat)
                   (get
                     subpat
                     'expand-pat-match-abbrev)))
                ((atom subpat)
                 subpat)
                (t
                 (cons (rec (first subpat))
                       (rec (rest subpat)))))))
      (rec pat))))              
   
#|

(pat-match-abbrev '?x* '(:* ?x))
-> (:* ?x)
   
(pat-match-abbrev '?y* '(:* ?y))
-> (:* ?y)
   
(setf axyd 
      (expand-pat-match-abbrev
       '(a ?x* ?y* d)))
-> (a (:* ?x) (:* ?y) d) |#
   
;;; A rule-based translation tool.
;;; Generalizes the pattern of 
;;; last chapter's use-eliza-rules.

(defun rule-based-translator
       (input rules &key 
         (matcher #'pat-match)
         (rule-if #'first)
         (rule-then #'rest)
         (action #'sublis)
         (var? #'variablep))
  "Find the first rule in rules that matches
  input, and apply the action to that rule."
  (some
   #'(lambda (rule)
       (let ((result 
               (funcall
                matcher
                (funcall rule-if rule)
                input
                +no-bindings+
                var?)))
         (unless (eq result +fail+)
             (funcall
              action
              result
              (funcall
               rule-then
               rule)))))
     rules))

#| 
  
Re-definition of use-eliza-rules using
rule-based-translator.

(defun use-eliza-rules (input)
  "Find some rule with which to transform
  the input."
  (rule-based-translator
   input
   *eliza-rules*
   :action #'(lambda (bindings responses)
               (sublis 
                 (switch-viewpoint bindings)
                 (random-elt responses)))))

|#

;;; A set of searching tools

;;; First, a tree-search tool. Data will
;;; be organized in trees.

(defun tree-search 
       (states goalp successors combiner)
  "tree-search 
    (states goalp successors combiner) -> obj
  states: list
  goalp: predicate function
  successors: function
  combiner: function.
  Find a state that satisfies goalp. Start
  with states and search according to 
  successors and combiner."
  (dbg :search "~&;; Search: ~A" states)
  (cond
    ((null states) +fail+)
    ((funcall goalp (first states))
     (first states))
    (t
      (tree-search
        (funcall 
          combiner
          (funcall
            successors
            (first states))
          (rest states))
        goalp
        successors
        combiner))))

(defun depth-first-search 
       (start goalp successors)
  "Search new states first until goal is
  reached."
  (tree-search 
    (list start) 
    goalp
    successors
    #'append)) 
              
(defun binary-tree (x)
  "Helper function for building a 
  binary tree of integers which can
  be called recursively to produce the
  elements (node indices) of the tree.
  Takes a single number to serve as a 
  local root."
  (list (* 2 x) (1+ (* 2 x))))

(defun is (value) ;1
  "Takes value and returns a function of
  one arg that checks the arg for equality
  with value. Uses EQL, so value must be 
  a symbol, number or character. With
  numbers, returns T only if the number
  has the same value and type. With 
  characters, the two compared characters
  must have the same character code--so
  the test is case sensitive."
  #'(lambda (x) (eql x value)))

#|

(debug* :search)
-> (:SEARCH)

(depth-first-search 1 (is 12) #'binary-tree)
[debug printout]
;; Search: (1)
;; Search: (2 3)
;; Search: (4 5 3)
;; Search: (8 9 5 3)
;; Search: (16 17 9 5 3)
;; Search: (32 33 17 9 5 3)
;; Search: (64 65 33 17 9 5 3)
;; Search: (128 129 65 33 17 9 5 3)
.
.
.
[infinite loop]
          
|#

(defun prepend (x y)
  "Prepend y to start of x."
  (append y x))

(defun breadth-first-search (start goalp successors)
  "Search old states first until goal
  is reached."
  (tree-search 
    (list start)
    goalp
    successors
    #'prepend))
    
#|

;; still in debugging mode

(breadth-first-search 
  1 
  (is 12) 
  #'binary-tree)
[debug output]
;; Search: (1)
;; Search: (2 3)
;; Search: (3 4 5)
;; Search: (4 5 6 7)
;; Search: (5 6 7 8 9)
;; Search: (6 7 8 9 10 11)
;; Search: (7 8 9 10 11 12 13)
;; Search: (8 9 10 11 12 13 14 15)
;; Search: (9 10 11 12 13 14 15 16 17)
;; Search: (10 11 12 13 14 15 16 17 18 19)
;; Search: (11 12 13 14 15 16 17 18 19 20 21)
;; Search: (12 13 14 15 16 17 18 19 20 21 22
            23)
[return]
-> 12
    
|#


(defun finite-binary-tree (n)
  "Return a successor function that 
  generates a binary tree with n nodes."
  #'(lambda (x)
      (remove-if
       #'(lambda (child)
           (> child n))
         (binary-tree x))))
         
#|

(depth-first-search 
  1 
  (is 12) 
  #'finite-binary-tree)

[debug]

;; Search (1)
          (2 3)
          (4 5 3)
          (8 9 5 3)
          (9 5 3)
          (5 3)
          (10 11 3)
          (11 3)
          (3)
          (6 7)
          (12 13 7)
-> 12
   
|#

(defun diff (num)
  "Return the function that finds the 
  difference from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter% (cost-fn) ; 1
  "Return a combiner function that sorts
  according to cost-fn."
  #'(lambda (new old)
      (sort 
       (append new old) 
       #'< 
       :key cost-fn)))

(defun best-first-search 
       (start goalp successors cost-fn)
  "Search lowest cost states first until
  goal is reached."
  (tree-search
   (list start)
   goalp
   successors
   (sorter cost-fn)))
   
#|

(best-first-search
 1
 (is 12)
 #'binary-tree
 (diff 12))

Search (1)
       (3 2)
       (7 6 2)
       (14 15 6 2)
       (15 6 2 28 29)
       (12 13 2 28 29 30 31)
-> 12
   
|#

(defun price-is-right (price)
  "Return a function that measures the 
  difference from price, but gives a big 
  penalty for going over price."
  #'(lambda (x)
      (if (> x price)
          most-positive-fixnum
          (- price x))))

#|
(best-first-search
 1
 (is 12)
 #'binary-tree
 (price-is-right 12))

;; Search (1)
          (3 2)
          (7 6 2)
          (6 2 14 15)
          (12 2 14 15 13)
-> 12
   
|#

(defun beam-search (start
                    goalp 
                    successors 
                    cost-fn 
                    beam-width)
  "Search highest scoring states first
  until goal is reached, but never consider
  more than beam-width states at a time."
  ; 1
  (tree-search 
    (list start)
    goalp
    successors
    #'(lambda (old new)
        (let ((sorted 
                (funcall 
                  (sorter cost-fn)
                  old
                  new)))
          (if (>= beam-width (length sorted))
              sorted
              (subseq
               sorted
               0
               beam-width))))))

#|

(beam-search 
  1
  (is 12)
  #'binary-tree
  (price-is-right 12)
  2)
;; Search: (1)
           (3 2)
           (7 6)
           (6 14)
           (12 13)
           
-> 12
   
(beam-search
 1
 (is 12)
 #'binary-tree
 (diff 12)
 2)

;; Search: (1)
           (3 2)
           (7 6)
           (14 15)
.
.
.
[infinite loop]
          
|#

;;; Using search to solve a problem: 
;;; planning routes by plane, in which
;;; the plane has a maximum range of 
;;; 1000 km per leg.

(defstruct (city (:type list))
  name
  long
  lat)

(defparameter *cities*
  '((atlanta 84.23 33.45)
    (boston 71.05 42.21)
    (chicago 87.37 41.50)
    (denver 105.00 39.45)
    (eugene 123.05 44.03)
    (flagstaff 111.41 35.13)
    (grand-jet 108.37 39.05)
    (houston 105.00 34.00)
    (indianapolis 86.10 39.46)
    (jacksonville 81.40 30.22)
    (kansas-city 94.35 39.06)
    (los-angeles 118.15 34.03)
    (memphis 90.03 35.09)
    (new-york 73.58 40.47)
    (oklahoma-city 97.28 35.26)
    (pittsburgh 79.57 40.27)
    (quebec 71.11 46.49)
    (reno 119.49 39.30)
    (san-francisco 122.26 37.47)
    (tampa 82.27 27.57)
    (victoria 123.21 48.25)
    (wilmington 77.57 34.14)))

(setf (symbol-function 'find-all-if)
      #'remove-if-not)
      
(defun neighbors (city)
  "Find all cities within 1000 km."
  (find-all-if
   #'(lambda (c)
       (and (not (eq c city))
            (< (air-distance c city) 1000)))
     *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(defun trip% (start dest) ;1
  "Search for a way from the start to dest."
  (beam-search
   start
   (is dest)
   #'neighbors
   #'(lambda (c) (air-distance c dest))
   1))

#| This functio provides routes, but not 
the most efficient ones. It minimizes each
leg of the trip instead of the whole trip.
It can give a different set of cities
for a trip from A to B versus a trip
from B to A. To fix this, we need to 
have intermediate information about 
the path.
|#

(defstruct (path 
             (:print-function print-path))
  state
  (previous nil)
  (cost-so-far 0)
  (total-cost 0))

;; We need not change our general 
;; tree functions to 
;; deal with paths. But we need to change
;; trip.

(defun trip (start dest 
             &optional (beam-width 1)) ;2
  "Search for the best path from start to
  dest."
  (beam-search
   (make-path :state start)
   (is dest :key #'path-state)
   (path-saver 
     #'neighbors 
     #'air-distance
     #'(lambda (c) (air-distance c dest)))
   #'path-total-cost
   beam-width))

(defconstant +earth-diameter+
  12765.0
  "Diameter of planet earth in km.")

(defun air-distance (city1 city2)
  "The great-circle distance between two
  cities."
  (let ((d 
          (distance (xyz-coords city1)
                    (xyz-coords city2))))
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coords of a point
  on a sphere. The center is (0 0 0)
  and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list
     (* (cos psi) (cos phi))
     (* (cos psi) (sin phi))
     (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two
  points in n-dimensional space."
  (sqrt
   (reduce #'+
     (mapcar #'(lambda (a b)
                 (expt (- a b) 2))
               point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (*
   (+
    (truncate deg)
    (* (rem deg 1) 100/60))
   pi
   1/180))

(defun is (value &key (key #'identity)
                      (test #'eql))
  "Returns a predicate that tests for a 
  given value."
  #'(lambda (path)
      (funcall 
        test 
        value 
        (funcall key path))))
        
(defun path-saver 
    (successors cost-fn cost-left-fn)
  "Returns a function that will take 
  a path as an arg and generate 
  successor paths, including costs."
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
         #'(lambda (new-state)
             (let ((old-cost
                    (+ (path-cost-so-far
                        old-path)
                       (funcall
                        cost-fn
                        old-state
                        new-state))))
               (make-path
                :state new-state
                :previous old-path
                :cost-so-far old-path
                :total-cost
                  (+ old-cost
                     (funcall
                       cost-left-fn
                       new-state)))))
           (funcall successors old-state)))))
   
(defun print-path 
       (path &optional (stream t) depth)
  (declare (ignore depth))
  (format 
    stream 
    "#<Path to ~A cost ~,1F>"
    (path-state path)
    (path-total-cost path)))

(defun show-city-path 
    (path &optional (stream t))
  "Show the length of a path, and the 
  cities on it."
  (format
    stream
    "#<Path ~,1F km: ~{~:(~A~)~^ - ~}>"
    (path-total-cost path)
    (reverse (map-path #'city-name path)))
  (values))

(defun map-path (fn path)
  "Call fn on each state in the path,
  collecting results."
  (if (null path)
      nil
      (cons (funcall fn (path-state path))
            (map-path
              fn (path-previous path)))))

;; While AI textbooks teach 
;; about algorithms that find the optimal
;; solution, these algorithms are often
;; impractical with large data sets. It
;; is often better to have an algorithm
;; that provides a close-to-optimal
;; solution quickly. Best-first search
;; is such an algorithm. Beam search
;; is tricky. The beam width greatly 
;; affects the results. The difference 
;; in the width greatly affects run 
;; time. It can quickly lead to the 
;; optimal solution, or it can fail
;; altogether. One fix is to start
;; with a narrow beam and, if there is
;; failure, widen the beam and try again.

(defun iter-wide-search
       (start goalp successors cost-fn
        &key (width 1) (max 100))
  "Search, increasing beam-width from
  width to max."
  (dbg :search "; Width: ~D" width)
  (unless (> width max)
    (or
     (beam-search 
       start 
       goalp 
       successors
       cost-fn
       width)
     (iter-wide-search
      start
      goalp
      successors
      cost-fn
      :width (1+ width)
      :max max))))

;; We are able to search our flights with
;; a tree search, because the graph can 
;; be construed as a tree. But this 
;; is inefficient, and potentially 
;; prone to infinite recursion. We 
;; need a dedicated function for 
;; searching a graph. The difference is
;; that our tree search has no facility
;; to avoid nodes it has already seen.

(defun graph-search% 
    (states goalp successors combiner
     &optional (state= #'eql) old-states)
     ; 1
  "Find a state that satisfies goalp. 
  Start with states, and search according
  to successors and combiner. Don't
  try the same state twice."
  (dbg :search "~&;; Search: ~A" states)
  (cond
   ((null states) +fail+)
   ((funcall goalp (first states))
    (first states))
   (t
    (graph-search
     (funcall
       combiner
       (new-states
         states
         successors
         state=
         old-states)
       (rest states))
     goalp
     successors
     combiner
     state=
     (adjoin 
       (first states) 
       old-states
       :test state=)))))

(defun new-states% ;1
       (states successors state= old-states)
  "Generate successor states that have not
  been seen before."
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)
           (member 
             state 
             old-states 
             :test state=)))
     (funcall successors (first states))))
  
(defun next2 (x)
  "Given a number x, returns the list
  (x+1, x+2). Used to demonstrate the 
  difference between tree-search and 
  graph search."
  (list (+ x 1) (+ x 2)))

#|

(debug* :search)
-> (:SEARCH)
(tree-search '(1) (is 6) #'next2 #'prepend)
[Debug printout]
;; Search (1)
          (2 3)
          (3 3 4)
          (3 4 4 5)
          (4 4 5 4 5)
          (4 5 4 5 5 6)
          (5 4 5 5 6 5 6)
          (4 5 5 6 5 6 6 7)
          (5 5 6 5 6 6 7 5 6)
          (5 6 5 6 6 7 5 6 6 7)
          (6 5 6 6 7 5 6 6 7 6 7)
-> 6
(graph-search '(1) (is 6) #'next2 #'prepend)
[debug printout]
       
;; Search (1)
          (2 3)
          (3 4)
          (4 5)
          (5 6)
          (6 7)
-> 6
   
|#

;; Now A* search, which weeds out whole
;; paths, in addition to already-seen
;; nodes. Paths are weeded out by cost.

(defun a*-search% (paths goalp successors
                  cost-fn cost-left-fn
                  &optional
                  (state= #'eql)
                  old-paths)
   ; 1
  "Find a path whose state satisfies
  goalp. Start with paths, and expand
  successors, exploring least cost first.
  When there are duplicate states, keep
  the one with the lower cost and 
  discard the other."
  (dbg :search ";;  Search: ~A" paths)
  (cond
   ((null paths)
    +fail+)
   ((funcall goalp (path-state (first paths)))
    (values (first paths) paths))
   (t
    (let* ((path (pop paths))
           (state (path-state path)))
      ;; Update paths and old-paths to
      ;; reflect the new successors of 
      ;; state.
      (setf old-paths
            (insert-path path old-paths))
      (dolist (state2 
               (funcall successors state))
        (let* ((cost (+
                      (path-cost-so-far path)
                      (funcall
                       cost-fn
                       state
                       state2)))
               (cost2 (funcall
                       cost-left-fn
                       state2))
               (path2 (make-path
                       :state state2
                       :previous path
                       :cost-so-far cost
                       :total-cost
                       (+ cost cost2)))
               (old nil))
          ;; Place the new path, path2, 
          ;; in the right list
          (cond
           ((setf old
                  (find-path
                   state2
                   paths
                   state=))
            (when (better-path path2 old)
              (setf paths
                    (insert-path
                     path2
                     (delete old paths)))))
           ((setf old 
                  (find-path
                   state2
                   old-paths
                   state=))
            (when (better-path path2 old)
              (setf paths
                    (insert-path
                     path2
                     paths))
              (setf old-paths
                    (delete old old-paths))))
           (t
            (setf paths
                  (insert-path
                   path2
                   paths))))))
      ;; Finally, call A* again with the 
      ;; updated path lists:
      (a*-search
       paths
       goalp
       successors
       cost-fn
       cost-left-fn
       state=
       old-paths)))))

(defun find-path% (state paths state=) ;1
  "Find the path with this state among
  the list of paths."
  (find 
    state 
    paths 
    :key #'path-state
    :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1)
     (path-total-cost path2)))

(defun insert-path% (path paths) ;1
  "Put path into the right position, 
  sorted by total cost."
  (merge 'list 
    (list path) 
    paths
    #'<
    :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (when path
    (cons 
      (path-state path)
      (path-states (path-previous path)))))
 
#|

(path-states
  (a*-search (list (make-path :state 1))
             (is 6)
             #'next2
             #'(lambda (x y) 1)
             (diff 6)))
[debug printout]
       
;; Search: (#<Path to 1 cost 0.0>)
           (#<Path to 3 cost 4.0> #<Path to 2 cost 5.0>)
           (#<Path to 5 cost 3.0> #<Path to 4 cost 4.0> #<Path to 2 cost 5.0>)
           (#<Path to 6 cost 3.0> #<Path to 7 cost 4.0> #<Path to 4 cost 4.0> #<Path to 2 cost 5.0>)
           
-> (6 5 3 1)   
              
|#

;; If we are careful with our goal predicate,
;; and the tree/graph is finite, we can
;; return a list of all paths to the 
;; "goal", not just the cheapest one

(defun search-all (start 
                   goalp 
                   successors 
                   cost-fn 
                   beam-width)
  "Find all solutions to a problem using
  beam search. Can lead to an infinite 
  loop."
  (let ((solutions nil))
    (beam-search
     start
     #'(lambda (x) 
         (when (funcall goalp x)
           (push x solutions))
         nil) ;; always fails
     successors
     cost-fn
     beam-width)
    (nreverse solutions)))

;;; GPS can be formulated as a beam search
;;; over the graph of the solution 
;;; space. If searching forward from the
;;; initial state, we can even avoid the
;;; Sussman anomaly.

#|

Exercise 6.3 [m] Write a version of 
interative-interpreter that is more general 
than the one defined in this chapter. 
Decide what features can be specified, and 
provide defaults for them.
        
|#

;; My version above is more general than
;; that presented in the text. The one
;; facility it lacks is a means of 
;; narrowing the error handling. But
;; such a change would require that we
;; we write a macro.

#|

Exercise 6.4 [m] Define a version of compose 
that allows any number of arguments, not 
just two. Hint: You may want to use the 
function reduce. |#
         
;; Alexandria provides such a function, 
;; so I will not reinvent the wheel. 
;; If I had to, I would write the 
;; following:
            
#|

(defun compose (fn1 &rest fns)
  (reduce
    #'(lambda (f g)
        #'(lambda (&rest args)
            (funcall f (apply g args))))
      fns
      :initial-value fn1))
      
|#

#|

Exercise 6.5 [m] Define a version of compose 
that allows any number of arguments but is 
more efficient than the answer to the 
previous exercise. Hint: try to make 
decisions when compose is called to build 
the resulting function, rather than making 
the same decisions over and over each time 
the resulting function is called. |#
    
#|

I had to look at the answer to see what 
was intended by the task prompt.
    
(defun compose (&rest fns)
  (case (length fns)
    (0 #'identity)
    (1 #'(lambda (&rest args)
           (apply (car fns) args)))
    (2 #'(lambda (&rest args)
           (funcall
            (car fns)
            (apply
             (cadr fns)
             args))))
    (otherwise
     (reduce
      #'(lambda (f g)
          #'(lambda (&rest args)
              (funcall
               f
               (apply g args)))
          fns)))))

|#

#|
Exercise 6.6 [m] One problem with pat-match 
is that it gives special significance to 
symbols starting with ?, which means that 
they can not be used to match a literal 
pattern. Define a pattern that matches the 
input literally, so that such symbols can be 
matched.
|#

;; See the the pattern matching section
;; where I have made the updates. pat-match
;; and expand-pat-match-abbrev were 
;; changed.

#|

Exercise 6.7 [m] Discuss the pros and cons of 
data-driven programming compared to the 
conventional approach.
             
|#

;; Pro: very easily extensible. Con: 
;; when definitions become significantly
;; abstract, they can be harder to read.

#|

Exercise 6.8 [m] Write a version of 
tree-search using an explicit loop rather 
than recursion. |#
     
#|
 
(defun tree-search
       (states goalp successors combiner)
  (do ((states-copy (copy-tree states)))
      ((null states-cooy) +fail+)
    (if (funcall goalp (first states-copy))
        (return (first states-copy))
        (setf
         states-copy
         (funcall
          combiner
          (funcall
           successors
           (first states-copy))
          (rest states-copy))))))
|#
 
#|

Exercise 6.9 [m] The sorter function is 
inefficient for two reasons: it calls append, 
which has to make a copy of the first 
argument, and it sorts the entire result, 
rather than just inserting the new states 
into the already sorted old states. Write a 
more efficient sorter. |#  
                          
(defun sorter (cost-fn) ; 2
  "Return a combiner function that sorts
  according to cost-fn."
  (lambda (old new)
    (merge 
      'list
      (sort new #'< :key cost-fn)
      old
      #'<
      :key cost-fn)))

#|

Exercise 6.10 [m] Write versions of 
graph-search and a*-search that use hash 
tables rather than lists to test whether a
state has been seen before. |#
      
 
(defun graph-search ;2
       (states goalp successor combiner
        &optional (state= #'eql)
                  (old-states
                   (make-hash-table
                     :test state=)))
  "Find a state that satisfies goalp. 
  Start with states, and search according
  to successors and combiner. Don't
  try the same state twice." 
  (cond
   ((null states) +fail+)
   ((funcall goalp (first states))
    (first states))
   (t
    (graph-search
     (funcall 
       combiner
       (new-states
        states
        successors
        state=
        old-states)
       (rest states))
     goalp
     successors
     combiner
     (progn
       (setf 
         (gethash 
           (first states) 
           old-states)
         t)
       old-states)))))

(defun new-states ;2
       (states successors state= old-states)
  "Generate successor states that have not
  been seen before."  
  (remove-if
   #'(lambda (state)
       (or
         (member state states :test state=)
         (gethash state old-states)))
     (funcall successors (first states))))

(defun a*-search (paths goalp successors
                  cost-fn cost-left-fn
                  &optional
                  (state= #'eql)
                  (test #'eql)
                  (old-states
                   (make-hash-table
                    :test test))) ;2
  "Find a path whose state satisfies
  goalp. Start with paths, and expand
  successors, exploring least cost first.
  When there are duplicate states, keep
  the one with the lower cost and 
  discard the other."  
  (cond
   ((null paths) +fail+)
   ((funcall goalp (path-state (first paths)))
    (values (first paths) paths))
   (t
    (let* ((path (pop paths))
           (state (path-state path)))
      (setf old-paths
            (insert-path path old-paths))
      (dolist (state2 
               (funcall successors state))
        (let* ((cost 
                 (+
                  (path-cost-so-far path)
                  (funcall
                   cost-fn
                   state
                   state2)))
               (cost2
                (funcall
                 cost-left-fn
                 state2))
               (path2
                (make-path
                 :state state2
                 :previous path
                 :cost-so-far cost
                 :total-cost
                 (+ cost cost2)))
               (old2 nil))
          (cond
           ((setf old
                  (find-path
                   state2
                   old-paths
                   state=))
            (when (better-path path2 old)
              (setf paths
                   (insert-path
                     path2
                     paths))
              (setf old-paths
                    (remhash old old-paths))))
           (t
            (setf paths
                  (insert-path
                   path2
                   paths))))))
      (a*-search
       paths
       goalp
       successors
       cost-fn
       cost-left-fn
       state=
       test
       old-paths)))))

(defun insert-path (path paths) ;2
  "Put path into the right position, 
  sorted by total cost."  
  (etypecase paths
    (list (merge 'list
            (list path)
            paths
            #'<
            :key #'path-total-cost))
    (hash-table 
      (setf (gethash path paths) t))))
            
 (defun find-path (state paths state=) ;2
   "Find the path with this state among
    the list of paths."
   (etypecase paths
     (list (find
            state
            paths
            :key #'path-state
            :test state=))
     (hash-table
       (loop
        for path being the hash-keys 
                           of paths
        when
        (funcall 
          state= 
          state 
          (path-state path))
        do
        (return path)))))
 
#|

Exercise 6.11 [m] Write a function that 
calls beam-search to find the first n 
solutions to a problem and returns them in 
a list.
  
|#

#|

(defun first-ten (start 
                  goalp 
                  successors
                  cost-fn
                  beam-width)
  (let ((count 1)
        (solutions nil))
    (beam-search
     start
     #'(lambda (state)
         (when (goalp state)
           (cond
            ((= count 10)
             (nreverse (push state results)))
            (t
             (push state results)
             (incf count)
             nil))))
      successors
      cost-fn
      beam-width)))
|#

(defmacro trace-all (&optional 
                      (package *package*))
  "Trace all user-created functions. 
  Optional arg ``package'', which
  defaults to *package*, specifies
  the package to trace."
  (let ((fns (gensym "FN-LIST"))
        (temp-fns nil))
    (loop
      for sym being the present-symbols in package
      when (fboundp sym)
      do
      (push sym temp-fns))
    `(let ((,fns ,temp-fns))
       (handler-case
         (trace ,@fns)
         (warning () nil)))))

               
   
    
  
       
            
 
     
     
   
            
           
           
          
                 

               
                 
                 
        
      
      
   
   
  
                  
                  
       
       
  
        
       
