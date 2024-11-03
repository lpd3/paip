;;;; paip/ch-15.lisp

;;;; Chapter 15: Symbolic Mathematics 
;;;; With Canonical Forms
                        
(in-package :canonical)

#| We wish to represent polynomials
by a canonical form under the hood.
1. Polynomials admit only addition and 
multiplication. 2. Polynomials are closed
under addition, subtraction, multiplication,
differentiation and integration. They are
not closed under division. If we 
restrict exponents to non-negative integers,
then exponentiation will be closed, as
well. 3. We imagine
most of our polynomials will be dense 
(like 5x^3 + 10x^2 + 20x + 40), rather than
sparse (like 5x^100 + 10x^50 + 20). 4. 
Our canonical form is a vector, with the 
first element as the main variable, and 
the subsequent entries are coefficients 
of terms of increasing degree. Pure numbers,
with no variables, are represented as 
themselves. So:
            
#(x 40 20 10 5) represents
                         
5x^3 + 10x^2 + 20x + 40
     
#(x 0 1) represents x

and

5 represents 5 
              
5. We use simple vectors, inlining of 
basic functions, and type declarations to
promote efficiency. |#
  
(proclaim '(inline main-var degree coef
                   var= var> poly make-poly))

(deftype polynomial () 'simple-vector)

(defun main-var (p)
  (svref (the polynomial p) 0))
  
(defun degree (p)
  (- (length (the polynomial p)) 2))

(defun coef (p i)
  (svref (the polynomial p) (1+ i)))

;; Main variables must be symbols. 
;; Coefficients may be numbers or 
;; polynomials.

(defun poly (x &rest coefs)
  "Make a polynomial with main variable 
  and coefficients in increasing order."
  (apply #'vector x coefs))

(defsetf main-var (p) (val)
  `(setf (svref (the polynomial ,p) 0) ,val))

(defun make-poly (x degree)
  "Make the polynomial 0 + 0x + 0x^2 + 
  0x^3 + ... + 0x^degree."
  (let ((p (make-array (+ degree 2)
             :initial-element 0)))
    (setf (main-var p) x)
    p))

(defsetf coef (p i) (val)
  `(setf 
     (svref 
       (the polynomial ,p) 
       (1+ ,i) )
     ,val))

(defun prefix->canon% (x) ; 1
  "Convert a prefix Lisp expression to 
  a polynomial canonical form. E.g.:
  (+ (^ x 2) (* 3 x)) => #(x 0 3 1);
  (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) 
  => 0"
  (cond
   ((numberp x) x)
   ((symbolp x) (poly x 0 1))
   ((and (expp x) ;; *
         (get (exp-op x) ;; *
              'prefix->canon))
    (apply (get (exp-op x) 'prefix->canon)
           (mapcar #'prefix->canon
                   (exp-args x)))) ;; *
   (t (error "Not a polynomial: ~A" x))))

;; Data-driven approach. Install the 
;; translations on the symbols

(dolist (item '((+ poly+) 
                (- poly-)
                (* poly*poly)
                (^ poly^n)
                (D deriv-poly)
                (I integ-poly)))
  (setf (get (first item) 'prefix->canon)
        (second item)))
        
(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) 
                  (second args)))))
                  
(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (ecase (length args)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly
        (first args)
        (poly*poly
         -1
         (second args))))))

#| To specify canonical forms, we require that
variables have an order. We simply use 
string> to determine the order. |#
        
(defun var= (x y)
  (eq x y))

(defun var> (x y)
  (string> x y))

;; Canonical form of (+ x y) or (+ y x) =>
;; #(x #(y 0 1) 1)
                 
(defun poly+poly (p q)
  "Add two polynomials."
  (normalize-poly
   (cond
    ((numberp p)
     (k+poly p q))
    ((numberp q)
     (k+poly q p))
    ((var= (main-var p) (main-var q))
     (poly+same p q))
    ((var> (main-var q) (main-var p))
     (k+poly q p))
    (t
     (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant, k, to a polynomial, p."
  (cond
   ((eql k 0)
    p)
   ((and (numberp k) (numberp p))
    (+ k p))
   (t
    (let ((r (copy-poly p)))
      (setf (coef r 0)
            (poly+poly (coef r 0) k))
      r))))

(defun poly+same (p q)
  "Add two polynomials with the same main 
  variable."
  ;; first assure that q is the higher 
  ;; degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;; Add each element of p into r, 
      ;; a copy of q.
      (let ((r (copy-poly q)))
        (loop for i from 0 to (degree p) do
          (setf (coef r i)
                (poly+poly
                 (coef r i)
                 (coef p i))))
        r)))

(defun copy-poly (p)
  "Make a copy of a polynomial"
  (copy-seq p))

(defun poly*poly (p q)
  "Muptiply two polynomials"
  (normalize-poly
   (cond
    ((numberp p)
     (k*poly p q))
    ((numberp q)
     (k*poly q p))
    ((var= (main-var p) (main-var q))
     (poly*same p q))
    ((var> (main-var q) (main-var p))
     (k*poly q p))
    (t
     (k*poly p q)))))

(defun k*poly (k p)
  "Multiply a polynomial, p, by a 
  constant factor, k."
  (cond
   ((eql k 0)
    0)
   ((eql k 1)
    p)
   ((and (numberp k)
         (numberp p))
    (* k p))
   (t
    (let ((r 
            (make-poly 
              (main-var p)
              (degree p))))
      ;; Accumulate result in r: 
      ;; r[i] = k*p[i]
      (loop for i from 0 to (degree p) do
        (setf (coef r i)
              (poly*poly
               k
               (coef p i))))
      r))))
      
(defun poly*same (p q)
  "Multiply two polynomials with the same
  variable."
  ;; r[i] = p[0]*q[i] + p[1]*q[i] + ...
  (let* ((r-degree (+ (degree p) (degree q)))
         (r (make-poly 
              (main-var p) 
              r-degree)))
    (loop for i from 0 to (degree p) do
      (unless (eql (coef p i) 0)
        (loop for j from 0 to (degree q) do
          (setf (coef r (+ i j))
                (poly+poly 
                  (coef r (+ i j))
                  (poly*poly
                   (coef p i)
                   (coef q j)))))))
    r))

(defun normalize-poly (p)
  "Alter a poly by deleting trailing zeros."
  (if (numberp p)
      p
      (let ((p-degree
              (- (position 
                   0
                   p
                   :test (complement #'eql)
                   :from-end t)
                 1)))
        (cond
         ((<= p-degree 0)
          (normalize-poly (coef p 0)))
         ((< p-degree (degree p))
          (delete 0 p :start p-degree))
         (t p)))))

(defun poly^n% (p n) ; 1
  "Raise polynomial to the nth power, 
  n >= 0."
  (check-type n (integer 0 *))
  (cond
   ((= n 0)
    (assert (not (eql p 0)))
    1)
   (t
    (poly*poly 
      p
      (poly^n p (- n 1))))))
      
;;; Differentiation

(defun deriv-poly (p x)
  "Return the derivative, dp/dx, of 
  the polynomial p."
  ;; If p is a number or a polynomial
  ;; with main-var > x, then p is free
  ;; of x, and the derivative is zero, 
  ;; else, do real work.
  ;; But first, make sure x is a 
  ;; simple variable, of the form
  ;; #(X 0 1)
  (assert (and (typep x 'polynomial)
               (= (degree x) 1)
               (eql (coef x 0) 0)
               (eql (coef x 1) 1)))
  (cond
   ((numberp p) 0)
   ((var> (main-var p) (main-var x)) 0)
   ((var= (main-var p) (main-var x))
   ;; d(a + bx + cx^2 + dx^3)/dx =
   ;; b + 2cx + 3dx^2, so shift the 
   ;; sequence p over by one, then put
   ;; x back in, and multiply by the 
   ;; exponents.
    (let ((r (subseq p 1)))
      (setf (main-var r) (main-var x))
      (loop for i from 1 to (degree r) do
        (setf (coef r i)
              (poly*poly (+ i 1) (coef r i))))
      (normalize-poly r)))
   (t
    ;; Otherwise, some coefficient may 
    ;; contain x. Ex.: 
    ;; d(z + 3x + 3zx^2 + z^2x^3)/dz =
    ;;   1 + 0  + 3x^2 + 2zx^3
    ;; So, copy p, and differentiate the 
    ;; coefficients.
    (let ((r (copy-poly p)))
      (loop for i from 0 to (degree p) do
        (setf (coef r i) (deriv-poly 
                           (coef r i)
                           x)))
      (normalize-poly r)))))

#| Ex. 15.1 Integrating polys is not 
   much harder than differentiating them.
   For example:
   ∫ax^2 + bx dx = (x^3)/3 + (bx^2)/2 + c
   Write a function to intrgrate 
   polynomials and install it into 
   prefix->canon
|#

;;; Since we are using the convention of
;;; comparing variables alphabetically, 
;;; and I don't want to bother with 
;;; examining each main variable
;;; to construct a suitable symbol 
;;; representing an integration constant, 
;;; I will use ~c to stand for that 
;;; constant. Note that ~ is the last of 
;;; the regular ASCII characters, so
;;; all variables should sort before 
;;; ~c....assuming we don't put tilde's
;;; at the beginning of our honest 
;;; variables.

(defun add-integ-const (p)
  (k+poly '(~c 0 1) p))

(defun integ-poly (p x)
  "Integration: ∫ p dx."
  ;; If p is a number, or 
  ;; (var> (main-var p) (main-var x)),
  ;; return px + c, that is
  ;; #(x #(~c 0 1) p), but first assert
  ;; that x is a plain variable.
  (assert (and (typep x 'polynomial)
               (= (degree x) 1)
               (eql (coef x 0) 0)
               (eql (coef x 1) 1)))
  ;; we want to make sure the integ
  ;; constant is added only once per
  ;; integration, but the inner integration
  ;; may be recursive
  (normalize-poly
    (add-integ-const
     (integ-aux p x))))

(defun integ-aux (p x)
  (cond
   ((or (numberp p)
        (var> (main-var p) (main-var x)))
    (poly (main-var x) 0 p))
   ((var= (main-var p) (main-var x))
    (let ((poly-args (list 0 (main-var x))))
      (dotimes (i (1+ (degree p)))
        (push (coef p i) poly-args))
      (let ((r (apply #'poly poly-args)))
        (loop for i from 2 to (degree r) do
          (setf (coef r i)
                (poly*poly (/ i) (coef r i))))
        r)))
   (t
    (let ((r (copy-poly p)))
      (loop for i from 0 to (degree r) do
        (setf (coef r i)
              (normalize-poly
                (integ-aux
                 (coef r i) x))))
      r))))

#|
Add support for definite integrals. You 
will need to make up a suitable notation
and properly install it in infix->prefix 
and prefix->canon. A full implementation
of this feature would have to consider 
infinity as a bound, as well as the 
problem of integrating over singularities.
You need not address these problems. |#
    
;;; Rather than creating a notation, I
;;; can build on the integ-poly function.

(defun eval-poly (p k)
  "Given a poly and a numerical constant, 
  return the result of substituting the 
  constant for the main variable in the 
  poly."
  (cond
   ((numberp p)
    p)
   (t
    (let ((result (coef p 0)))
      (loop for power from 1 to (degree p) do
        (incf result (* (coef p power)
                        (expt k power))))
      result))))

(defun poly-calc-interval (p a b)
  "Given a polynomial, and two endpoints, 
  where a is the lower endpoint, and b 
  the upper, return the difference 
  of evaluating p with b and with a."
  (- (eval-poly p b)
     (eval-poly p a)))

(defun def-integ (p a b)
  "Given a polynomial and the endpoints
  of the integration interval, return 
  the definite integral of the polynomial,
  evaluate along the interval."
  (let ((integ (integ-poly p (main-var p))))
    ;; get rid of the dummy constant
    (setf (coeff integ 0) 0)
    (poly-calc-interv integ a b)))
    
#| Converting between infix and prefix 
notation. A new function that permits 
multiple args. |#
         
(defun prefix->infix (exp)
  "Translate prefix to infix expressions.
  Handles operators with any number of 
  args."
  (if (atom exp)
      exp
      (intersperse
       (exp-op exp)
       (mapcar #'prefix->infix 
         (exp-args exp)))))

(defun intersperse (op args)
  "Place op between each element of args.
  Ex.: (intersperse '+ '(a b c)) => 
  (a + b + c)"
  (if (length=1 args) ; *
      (first args)
      (rest
       (loop for arg in args
         collect op
         collect arg))))
         
#| Convert from canonical form to 
prefix form |#
             
(defun canon->prefix (p)
  "Convert a canonical polynomial to a 
  Lisp expression."
  (if (numberp p)
      p
      (args->prefix
       '+ 0
       (loop for i from (degree p) downto 0
         collect 
           (args->prefix 
             '* 1
             (list 
               (canon->prefix (coef p i))
               (exponent->prefix
                (main-var p) i)))))))

(defun exponent->prefix (base exponent)
  "Convert canonical base^exponent to 
  prefix form"
  (case exponent
    (0 1)
    (1 base)
    (t `(^ ,base ,exponent))))

(defun args->prefix (op identity args)
  "Convert arg1 op arg2 op ... op argn
  to (op arg1 arg2 ... argn)."
  (let ((useful-args (remove identity args)))
    (cond
     ((null useful-args) identity)
     ((and (eq op '*) (member 0 args)) 0)
     ((length=1 args)
      (first useful-args))
     (t
      (cons 
        op 
        (mappend
         #'(lambda (exp)
             (if (starts-with exp op)
                 (exp-args exp)
                 (list exp)))
           useful-args))))))
           
#| top-level functions for access |#
   
   
(defun canon (infix-exp)
  "Canonicalize argument and convert it 
  back to infix."
  (prefix->infix
   (canon->prefix
    (prefix->canon
     (infix->prefix infix-exp)))))

(defun canon-simplifier ()
  "Read an expression, canonicalize it, 
  and print the result."
  (let* ((output
          (make-broadcast-stream
           *standard-output*
           *query-io*))
         (*standard-output* output))
   (loop
    (print 'canon>)
    (let ((input (read)))
      (if (eq input 'quit)
          (return)
          (print (canon input)))))))
          
#| Unfortunately, this doesn't quite work.
We need a new infix->prefix function that
can handle nested expressions.
    
This is adapted from 
     http://www.lispology.com/show?JIH
     
|#

(defparameter *binary-operators*
  '((D . 0)
    (E . 0)
    (+ . 1)
    (- . 1)
    (* . 2)
    (^ . 3)))

(defparameter *unary-operators*
  '((+ . 4)
    (- . 4)))
    
(defun weight (c)
  (rest
   (assoc c *binary-operators*)))

(defun infix->prefix (expr)
  (if (atom expr)
      expr
      (inf-aux expr nil nil)))

(defun inf-aux (expr operators operands)
  (cond
   ;; Unary operator
   ((and (atom (first expr))
         (assoc 
           (first expr) 
           *unary-operators*))
    (inf-iter 
      (rest (rest expr))
      operators
      (cons
       (list
        (first expr)
        (infix->prefix
           (first (rest expr))))
       operands)))
   (t
    (inf-iter
     (rest expr)
     operators
     (cons
      (infix->prefix (first expr))
      operands)))))

(defun inf-iter (expr operators operands)
  (cond
   ((and (null expr)
         (null operators))
    (first operands))
   ((and expr
         (or (null operators)
             (> (weight (first expr))
                (weight (first operators)))))
   (inf-aux 
     (rest expr)
     (cons
      (first expr)
      operators)
     operands))
   (t
    (inf-iter 
      expr 
      (rest operators)
      (cons
       (list01
        (first operators)
        (first (rest operands))
        (first operands))
       (rest (rest operands)))))))

;; This works beautifully with the 
;; examples in the book. I did most of 
;; them. The following section changes
;; polynomial exponentiation to make 
;; it more efficient, using the 
;; binomial theorem. I'm going to 
;; skip that.    

#| Next, an extension to Rational 
Expressions |#
            
(defun make-rat (numerator denominator)
  "Build a rational: a quotient of 
  two polynomials."
  (if (numberp denominator)
      (k*poly (/ denominator) numerator)
      (cons numerator denominator)))

(defun rat-numerator (rat)
  "The numerator of a rational expression"
  (typecase rat
    (cons (car rat))
    (number (numerator rat))
    (t rat)))

(defun rat-denominator (rat)
  "The denominator of a rational expression"
  (typecase rat
    (cons (cdr rat))
    (number (denominator rat))
    (t 1)))

#| Exercise 15.3
   
Modify prefix->canon to accept input of the
form x / y and to return rational expressions
instead of polynomials. Also allow input of
the form x ^ (- n). |#
    
(defun prefix->canon (x) ; 1
  "Convert a prefix Lisp expression to 
  a polynomial canonical form. E.g.:
  (+ (^ x 2) (* 3 x)) => #(x 0 3 1);
  (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) 
  => 0. Improved version that accepts 
  rationals. In addition to already 
  accepted inputs, it now accepts symbols
  containing / or a list of 3 elements 
  with / as the second element. Does
  not exclusively return rational expressions,
  as this would make things too cumbersome."
  (cond
   ((numberp x) x)
   ((and (symbolp x) (position #\/ (symbol-name x)))
    (let* ((sname (symbol-name x))
           (/idx (position #\/ sname)))
      (assert (and
               (= /idx (position #\/ sname :from-end t))
               (char/= (char sname 0) #\/)
               (char/= (char sname (1- (length sname)))
                       #\/))
              ()
              "Bad rational: %S" x)
      (cond
       ((every #'digit-char-p 
          (subseq sname 0 /idx))
        (make-rat 
          (parse-integer
            (subseq sname 0 /idx))
          (prefix->canon
           (intern
            (subseq sname (1+ /idx))))))
       ((every #'digit-char-p
          (subseq sname (1+ /idx)))
        (make-rat
         (prefix->canon
          (intern
           (subseq sname 0 /idx)))
         (parse-integer
          (subseq sname (1+ /idx)))))
       (t
        (make-rat
         (prefix->canon
          (intern
           (subseq sname 0 /idx)))
         (prefix->canon
          (intern
           (subseq sname (1+ /idx)))))))))
   ((symbolp x) (poly x 0 1))
   ((and (consp x)
         (= (length x) 3)
         (eq (second x) '/))
    (make-rat
     (prefix->canon (first x))
     (prefix->canon (third x))))
   ((and (expp x) ;; *
         (get (exp-op x) ;; *
              'prefix->canon))
    (apply (get (exp-op x) 'prefix->canon)
           (mapcar #'prefix->canon
                   (exp-args x)))) ;; *
   (t (error "Not a polynomial: ~A" x))))

(defun poly^n (p n) ; 2
  "Raise polynomial to the nth power."
  (check-type n integer)
  (cond
   ((= n 0)
    (assert (not (eql p 0)))
    1)
   ((> n 0)
    (poly*poly 
      p
      (poly^n p (- n 1))))
   (t
    (make-rat 1 (poly^n p (- n))))))

#| Exercise 15.4
   
Add arithmetic routines for multiplication, 
addition, and division of rational 
expressions. Call them rat*rat, rat+rat and 
rat/rat, respectively. They will call 
upon poly*poly, poly+poly, and a new function,
poly/poly, which will be defined in the 
next two exercises.
     
|#
 
(defun polyp (x)
  (or (vectorp x)
      (numberp x)))
     
(defun proper-poly-p (x)
  (vectorp x))

(defun rat*rat (r1 r2)
  (cond
   ((or (and (numberp r1)
             (zerop r1))
        (and (numberp r2)
             (zerop r2)))
    0)
   ((and (numberp r1) (= r1 1))
    r2)
   ((and (numberp r2) (= r2 1))
    r1)
   ((and
     (polyp r1)
     (polyp r2))
    (poly*poly r1 r2))
   ((polyp r1)
    (rat*rat r2 r1))
   ((polyp r2)
    (make-rat
     (poly*poly 
       (rat-numerator r1)
       r2)
     (rat-denominator r1)))
   (t
    (make-rat
     (poly*poly
      (rat-numerator r1)
      (rat-numerator r2))
     (poly*poly
      (rat-denominator r1)
      (rat-denominator r2))))))

(defun rat+rat (r1 r2)
  (cond
   ((and (numberp r1) (zerop r1))
    r2)
   ((and (numberp r2) (zerop r2))
    r1)
   ((and (polyp r1)
         (polyp r2))
    (poly+poly r1 r2))
   ((polyp r1)
    (rat+rat r2 r1))
   ((polyp r2)
    (make-rat
     (poly+poly
      (rat-numerator r1)
      (poly*poly
       r2
       (rat-denominator r1)))
     (rat-denominator r1)))
   (t
    (make-rat
     (poly+poly
      (poly*poly
       (rat-numerator r1)
       (rat-denominator r2))
      (poly*poly
       (rat-numerator r2)
       (rat-denominator r1)))
     (poly*poly
      (rat-denominator r1)
      (rat-denominator r2))))))

(defun rat/rat (r1 r2)
  (cond
   ((and (polyp r1)
         (polyp r2))
    (poly/poly r1 r2))
   ((polyp r2)
    (rat*rat
     r1
     (poly/poly
       1
       r2)))
   ((zerop (rat-numerator r2))
    (error
     "Division by 0 rat"))
   (t
    (rat*rat
     r1
     (make-rat
      (rat-denominator r2)
      (rat-numerator r2))))))

#| Exercise 15.5
Define poly-gcd, which computes the greatest
common divisor of two polynomials.
|#

#| Define poly/poly, which will implement
division for polynomials. Polynomials are
not closed under division, so the function
will return a rational expression.
|#
 
(defun poly= (p1 p2)
  "Are these polys equal?"
  (or (and (numberp p1)
           (numberp p2)
           (= p1 p2))
      (and
        (proper-poly-p p1)
        (proper-poly-p p2)
        (var= (main-var p1) (main-var p2))
        (= (degree p1) (degree p2))
        (every 
        #'(lambda (coef-idx)
            (poly=
              (coef p1 coef-idx)
              (coef p2 coef-idx)))
          (iota (1+ (degree p1)))))))

(defun poly/poly (dividend divisor)
  "Division of polys. Returns two
  values: the quotient (a poly or rat) and 
  the remainder (a rat or 0)."
  (cond
   ((and (numberp dividend)
         (numberp divisor))
    (values (/ dividend divisor) 0))
   ((numberp dividend)
    (if (zerop dividend)
        (values 0 0)
        (values
         0
         (make-rat dividend divisor))))
   ((numberp divisor)
    (assert (not (zerop divisor))
      ()
      "DIVISION BY ZERO: (poly/poly ~S 0)"
      dividend)
    (if (= divisor 1)
        (values dividend 0)
        (values
         (poly*poly
          dividend
          (/ 1 divisor))
         0)))
   ((var> (main-var dividend)
          (main-var divisor))
    (values
     (poly*poly
      dividend
      (poly/poly
       1
       divisor))
     0))
   ((var> (main-var divisor)
          (main-var dividend))
    (values
     0
     (make-rat divisor dividend)))
   ((or (notevery
         #'(lambda (ci)
             (numberp (coef dividend ci)))
           (iota (1+ (degree dividend))))
        (notevery
         #'(lambda (ci)
             (numberp (coef divisor ci)))
           (iota (1+ (degree divisor)))))
    (values (make-rat dividend divisor) 0))
   (t (expanded-synthetic-division
       dividend divisor))))

(defun expanded-synthetic-division
       (dividend divisor)
  "Fast algorithm to divide two proper 
  polynomials with the same main 
  variable. Returns two values: a 
  quotient (poly) and a remainder 
  (rat or 0). See the Wikipedia article
  Synthetic Division."
  (let* ((deg1 (degree dividend))
         (deg2 (degree divisor))
         (divisor-coefs 
           (make-array (1+ deg2)
             :element-type 'number))
         (out
            (make-array (1+ deg1)
             :element-type 'number))
         (normalizer
          (coef divisor (degree divisor))))
    (do ((i deg2 (1- i))
         (j 0 (1+ j)))
        ((> j deg2))
      (setf (aref divisor-coefs j)
            (coef divisor i)))
    (do ((i deg1 (1- i))
         (j 0 (1+ j)))
        ((> j deg1))
      (setf (aref out j)
            (coef dividend i)))
    (dotimes (i (1+ (- deg1 deg2)))
      (setf (aref out i)
            (/ (aref out i) normalizer))
      (let ((coef (aref out i)))
        (when (/= coef 0)
          (do ((j 1 (1+ j)))
              ((> j deg2))
            (incf (aref out (+ i j))
                  (*
                   (-
                    (aref divisor-coefs j))
                   coef))))))
    (let ((separator 
            (mod (- 1 deg2) (length out)))
          (quotient-list nil)
          (remainder-list nil))
      (dotimes (i separator)
        (push (aref out i) quotient-list))
      (do ((i separator (1+ i)))
          ((= i (length out)))
        (push (aref out i) remainder-list))
      (let ((quotient
             (apply
              #'poly
              (main-var dividend)
              quotient-list))
            (remainder
             (if (every 
                   #'zerop 
                   remainder-list)
                 0
                 (make-rat
                  (apply 
                    #'poly
                    (main-var dividend)
                    remainder-list)
                  divisor))))
        (values quotient remainder)))))
                     
(defun ratp (x)
  (and (consp x)
       (let ((rst (cdr x)))
         (and (atom rst)
              (not (null rst))))))
     
(defun rat= (r1 r2)
  (or
    (and (polyp r1) 
         (polyp r2)
         (poly= r1 r2))
    (and (ratp r1)
         (ratp r2)
         (poly= (rat-numerator r1)
                (rat-numerator r2))
         (poly= (rat-denominator r1)
                (rat-denominator r2)))))  
 




