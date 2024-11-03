;;;; paip/ch-03.lisp
                  
;;;; Chapter 3: An Overview of Lisp 

(in-package :lisp-overview)
            
#| Exercise 3.1 [m] Show a lambda expression 
that is equivalent to the above let* 
expression. You may need more than one 
lambda. |#
        
;; The original expression:
;; (let* ((x 6)
;;        (y (* x x)))
;;   (+ x y))
            
;; Using lambda: (assuming x is passed)
;; (lambda (x) (+ x (* x x)))

#| Exercise 3.2 [s] The function cons can be 
seen as a special case of one of the other 
functions listed previously. Which one? |#
          
;; According to the answer key, list*

#| Exercise 3.3 [m] Write a function that 
will print an expression in dotted pair 
notation. Use the built-in function princ 
to print each component of the expression. |#
 
(defun dot (expr)
  (cond
    ((atom expr)
     (princ expr))
    (t
     (princ "(")
     (dot (car expr))
     (princ " . ")
     (dot (cdr expr))
     (princ ")"))))
     
#|

Exercise 3.4 [m] Write a function that, 
like the regular print function, will print 
an expression in dotted pair notation when 
necessary but will use normal list notation 
when possible. |#
                
(defun proper-list-p (obj)
  (cond
   ((null obj)
    t)
   ((atom obj)
    nil)
   (t
    (do ((rest obj (cdr rest)))
        ((null rest) t)
      (when (atom rest)
        (return nil))))))

(defun dot2 (obj)
  (cond
   ((null obj)
    nil)
   ((atom obj)
    (princ obj))
   ((proper-list-p obj)
    (princ "(")
    (do* ((len (length obj))
          (i 0 (1+ i))
          (elt #1=(nth i obj) #1#))
         ((= i len) (princ ")"))
      (unless (zerop i)
        (princ " "))
      (if (atom elt)
          (princ elt)
          (dot2 elt))))
   (t
    (princ "(")
    (do ((rest obj (rest rest))
         (i 0 (incf i)))
        ((atom rest)
         (progn
           (princ " . ")
           (princ (nthcdr i obj))
           (princ ")")))
      (unless (zerop i)
        (princ " "))
      (if (atom (first rest))
          (princ (first rest))
          (dot2 (first rest)))))))

#| Exercise 3.6 [s] Given the following 
initialization for the lexical variable a 
and the special variable *b*, what will be 
the value of the let form?
    
(setf a 'global-a)
(defvar *b* 'global-b)
(defun fn () *b*)
(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) 
    (symbol-value 'a) 
    (symbol-value '*b*)))
|#

;; (LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B)
     
#| Exercise 3.9 [m] Write a version of length 
using the function reduce. |#
      
(defun red-len (lst)
  (reduce
   #'(lambda (l _) (1+ l))
     lst
     :initial-value 0))

#| Exercise 3.10 [m] Use a reference manual 
or describe to figure out what the functions 
lcm and nreconc do. |#
    
;; lcm: given two integers, return their 
;; least common multiple.

;; nreconc: Given two lists, destructively
;; reverses the first, and then destructively
;; adds the elements of the second to the end
;; of the reversed first.

#| Exercise 3.11 [m] There is a built-in 
Common Lisp function that, given a key, a 
value, and an association list, returns a 
new association list that is extended to 
include the key-value pair. What is the 
name of this function? |#
     
;; acons