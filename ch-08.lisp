;;;; paip/ch-08.lisp
                   
;;;; Chapter 8: Symbolic Mathematics:
;;;; A Simplification Program
                            
(in-package :symbolic)

;;; This chapter is inspired by, but 
;;; does not emulate, the major 
;;; implementations of symbolic 
;;; math, viewed as a supreme 
;;; challenge to AI: SAINT (1963),
;;; SIN and, eventually MACSYMA 
;;; (1970s), which completely solved
;;; the problem. Today, many computer-based
;;; symbolic algebra programs exist. Since
;;; the entire problem is solved and all the 
;;; necessary algorithms are written, 
;;; symbolic algebra is no longer an
;;; active area of AI research.

;;; Rather than develop our own similar
;;; system, a formidable task, we will
;;; work on a tiny subproject called
;;; simplify, which simplifies symbolic
;;; algebra expressions.

;; We will not use regular rules, but 
;; instead, expressions. For clarity, 
;; the expressions will be written in 
;; infix notation, and we will convert
;; to prefix notation behind the scenes.
                                       
(defparameter d 'd)

(defparameter int 'int)

(defparameter exp-lhs 'exp-lhs)

(defparameter exp-rhs :exp-rhs)

(defparameter exp-op 'exp-op)

(defparameter x 'x)

(defparameter y 'y)

(defparameter undefined 'undefined)

(defparameter e 'e)

(defun infix->prefix% (infix-exp) ; 1
  "Convert fully-parenthesized infix-exp
  to a prefix expression"
  ;; Don't use this with non-fully 
  ;; parenthesized expressions
  (prefix->infix infix-exp)) 
                            
(defun infix->prefix (exp) ; 2
  "Translate an infix expression into
  prefix notation."
  ;; Note we cannot do implicit 
  ;; multiplication in this system
  (cond
   ((atom exp)
    exp)
   ((= (length exp) 1)
    (infix->prefix (first exp)))
   ((rule-based-translator
     exp
     *infix->prefix-rules*
     :rule-if #'rule-pattern
     :rule-then #'rule-response
     :action
     #'(lambda (bindings response)
         (sublis
          (mapcar
           #'(lambda (pair)
               (cons
                (first pair)
                (infix->prefix
                 (rest pair))))
             bindings)
          response))
      :var? #'variablep))
   ((symbolp (first exp))
    (list
     (first exp)
     (infix->prefix (rest exp))))
   (t
    (error "Illegal expression ~%~A~%
           INFIX->PREFIX" exp))))

(defun variablep (exp)
  "Variables are the symbols M through Z."
  ;; put x y z first to find them faster
  (member exp
    '(x y z m n o p q r s t u v w)))

(pat-match-abbrev 'x+ '(?+ x))

(pat-match-abbrev 'y+ '(?+ y))

(defun rule-pattern (rule)
  (first rule))

(defun rule-response (rule)
  (second rule))

(defparameter *infix->prefix-rules*%
  (mapcar #'expand-pat-match-abbrev
    '(((x+ = y+) (= x y))
      ((- x+) (- x))
      ((+ x+) (+ x))
      ((x+ + y+) (+ x y))
      ((x+ - y+) (- x y))
      ((x+ * y+) (* x y))
      ((x+ / y+) (/ x y))
      ((x+ ^ y+) (^ x y))
      ((log x+) (log x))
      ((sin x+) (sin x))
      ((cos x+) (cos x))))
  "A list of rules, ordered by precedence.")
  
(defparameter *infix->prefix-rules* ; 2
  (mapcar
   #'expand-pat-match-abbrev
   '(((x+ = y+) (= x y))
     ((- x+) (- x))
     ((+ x+) (+ x))
     ((x+ + y+) (+ x y))
     ((x+ - y+) (- x y))
     ((d y+ / d x) (d y x))
     ((int y+ d x) (int y x))
     ((x+ * y+) (* x y))
     ((x+ / y+) (/ x y))
     ((x+ ^ y+) (^ x y))
     ((log x+) (log x))
     ((sin x+) (sin x))
     ((cos x+) (cos x)))))

(defstruct (rule (:type list))
  (pattern nil)
  (response nil))

(defstruct (exp (:type list)
                (:constructor
                 mkexp
                 (&optional
                  lhs
                  op
                  rhs)))
  (op nil)
  (lhs nil)
  (rhs nil))

(defun expp (x)
  (consp x))

(defun exp-args (x)
  (rest x))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp)
      exp
      (mapcar #'prefix->infix
        (if (binary-exp-p exp)
            (list
             (exp-lhs exp)
             (exp-op exp)
             (exp-rhs exp))
            exp))))

(defun binary-exp-p (x)
  (and (expp x)
       (= (length (exp-args x)) 2)))

(defparameter *simplification-rules*
  (mapcar #'infix->prefix
    '((x + 0 = x)
      (0 + x = x)
      (x + x = 2 * x)
      (x - 0 = x)
      (0 - x = - x)
      (x - x = 0)
      (- - x = x)
      (x * 1 = x)
      (1 * x = x)
      (x * 0 = 0)
      (0 * x = 0)
      (x * x = x ^ 2)
      (x / 0 = undefined)
      (0 / x = 0)
      (x / 1 = x)
      (x / x = 1)
      (0 ^ 0 = undefined)
      (x ^ 0 = 1)
      (0 ^ x = 0)
      (1 ^ x = 1)
      (x ^ 1 = x)
      (x ^ -1 = 1 / x)
      (x * (y / x) = y)
      ((y / x) * x = y)
      ((x * y) / x = y)
      ((y * x) / x = y)
      (x + - x = 0)
      ((- x) + x = 0)
      (x + y - x = y))))

(defun simplifier ()
  "Read a mathematical expression, 
  simplify it and return the result."
  (loop
    (print 'simplifier> *query-io*)
    (let ((submission (read)))
      (when (eq submission 'q)
        (return 'done))
      (write 
        (simp submission) 
        :stream *query-io*
        :right-margin 60))))

(defun simp (inf)
  (prefix->infix
   (simplify (infix->prefix inf))))

(defun simplify (exp)
  "Simplify an expression by first 
  simplifying its components."
  (if (atom exp)
      exp
      (simplify-exp
        (mapcar #'simplify exp))))

(defun simplify-exp% (exp) ; 1
  "Simplify using a rule, or by doing
  arithmetic."
  (cond
   ((rule-based-translator
     exp
     *simplification-rules*
     :rule-if #'exp-lhs
     :rule-then #'exp-rhs
     :action
       #'(lambda (bindings response)
           (simplify
            (sublis
             bindings
             response)))
     :var? #'variablep))
   ((evaluable exp) (eval exp))
   (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that
  can be evaluated?"
  (and
   (every #'numberp (exp-args exp))
   (or
    (member (exp-op exp) '(+ - * /))
    (and
     (eq (exp-op exp) '^)
     (integerp (second (exp-args exp)))))))

(defun ^ (x y)
  (expt x y))

#|

(simplifier)
printout-> SIMPLIFIER>
(2 + 2)
printout-> 4
           SIMPLIFIER>
(5 * 20 + 30 + 7)
printout-> 137
           SIMPLIFIER>
(5 * x - (4 + 1) * x)
printout-> 0
           SIMPLIFIER>
(y / z * (5 * x - (4 + 1) * x))
printout-> 0
           SIMPLIFIER>
((4 - 3) * x + (y / y - 1))
printout-> X
           SIMPLIFIER>
(1 * f(x) + 0)
printout-> (F X)
           SIMPLIFIER>
(3 * 2 * x)
printout-> (3 * (2 * X))
           SIMPLIFIER
q
-> NIL
   
;; The program handles pure arithmetic 
;; well. It also did a good job with the
;; first few simplications. The last one
;; shows undesirable behavior. We would
;; like the simplification to be 
;; (6 * X) rather than (3 * (2 * X)).
|#

;; What we need is for the program to 
;; use associativity and commutivity and 
;; perform operations
;; with an option to change groupings.
;; This can be done with pat-match, 
;; using the :is pattern. 

;; It would seem at first that we could 
;; fix the snag with one additional 
;; expression rule. But it would not solve 
;; the problem for expressions with different
;; shapes. Instead, we need to adopt three
;; new principles:

;; 1a. In products with a variable and a 
;; number,
;; the number must come first (not at top
;; level, but behind the scenes.)
;; Thus (* x 3) -> (* 3 x). 
                           
;; 1b. In sums with a variable and a 
;; number, the variable must come first.
;; Thus (1 + x) -> (x + 1)
                         
;; 2. Group an outer number with an 
;; inner number. Thus 3 * (5 * x) ->
;; (3 * 5) * x.

;; 3. Move numbers from inner expressions
;; to outer expressions wherever possible.
;; Thus (3 * x) * y -> 3 * (x * y)
                                 
;; Define n and m as numbers, s as a 
;; non-number


(defun clear-plists (&rest syms)
  "Sets the plist of symbols to
  nil. Takes an arbitrary number of
  symbols."
  (dolist (s syms)
    (setf (symbol-plist s) nil))
  'ok)
              
;; new abbreviations for vars that
;; may only stand for numbers and for
;; those that must not
                     
(defun clear-props (&rest symbs)
  (dolist (symb symbs 'ok)
    (setf (symbol-plist symb) nil)))

(clear-props '?n '?m '?s)

(pat-match-abbrev '?n '(?is n numberp))

(pat-match-abbrev '?m '(?is m numberp))

(pat-match-abbrev '?s '(?is s not-number-p))

(defun not-number-p (x)
  (not (numberp x)))

(defun simp-rule (rule)
  "Transform a rule into proper format."
  (let ((exp (infix->prefix rule)))
    (mkexp
     (expand-pat-match-abbrev (exp-lhs exp))
     (exp-op exp)
     (exp-rhs exp))))

(setf *simplification-rules*
  (append *simplification-rules*
    (mapcar #'simp-rule
      '((?s * ?n = n * s)
        (?n * (?m * x) = (n * m) * x)
        (x * (?n * y) = n * (x * y))
        ((?n * x) * y = n * (x * y))
        (?n + ?s = s + n)
        ((x + ?m) + ?n = x + (n + m))
        (x + (y + ?n) = (x + y) + n)
        ((x + ?n) + y = (x + y) + n)))))

#|

(simplifier)
po-> SIMPLIFIER>
(3 * 2 * x)
po-> (6 * X)
     SIMPLIFIER>
(2 * x * x * 3)
po-> (6 * (X ^ 2))
     SIMPLIFIER>
(2 * x * 3 * y * 4 * z * 5 * 6)
po-> (720 * (X * (Y * Z)))
     SIMPLIFIER>
(3 + x + 4 + x)
po-> ((2 * X) + 7)
     SIMPLIFIER>
(2 * x * 3 * x * 4 * (1 / x) * 5 * 6)
po-> (* 720 X)
     SIMPLIIFIER>
(3 + x + 4 - x)
po-> 7
     SIMPLIFIER>
;; Great! But....
(x + y + y + x)
po-> (X + (Y + (Y + X)))
     SIMPLIFIER>
;; and
(3 * x + 4 * x)
po-> ((3 * X) + (4 * X))
     SIMPLIFIER>
;; The last two should be (2 * (X + Y))
;; and (7 * X)
             
;; We will fix these in chapter 8.
|#

#|

Exercise 8.1 Verify that the set of rules 
just prior does indeed implement the 
desired conventions, and that the 
conventions have the proper effect, and 
always terminate. As an example of a 
potential problem, what would happen if we 
used the rule (x * n = n * x) instead of 
the rule (s * n = n * s)? |#
    
#| First--do the rules implement the 
desired conventions?
  s * n = n * s. 
     This reverses
     q * 5, as desired. It does not 
     touch 5 * 5 or q * (3 * 7).
     Correct.
  n * (m * x) = (n * m) * x
     Since the pat-match function is 
     recursive, it digs into the nested
     sub-expressions first. By the time
     we get to the outer expression, the
     inner one will have been adjusted
     to be in the right order. So there
     is no need to write a rule for
     n * (s * m), for example.
     Now x here can stand for anything.
     thus (m * x) could be (7 * 5)
     or (7 * q) or (7 * (43 + (8 * 3 + z))).
     Since we leave x alone, we need not
     worry about its type at the outer
     expression level. But it does pull
     the two numbers together. 
     Correct.
   x * (n * y) = n * (x * y)
     Since this rule appears after the 
     previous one, x will not be a number
     in this rule. The rule gets the number
     out of parens. Correct.
   (n * x) * y = n * (x * y)
     Correct
   n + s = s + n
     Correct
   (x + m) + n = x + n + m
     Correct. I'm not sure if the 
     lack of parentheses on the right
     is correct, or a misprint. However,
     this works. So it appears to be ok.
   x + (y + n) = (x + y) + n
     Correct
   (x + n) + y = (x + y) + n
     Correct.
     
   Now, does it give the results we desire?
   Yes, according to our tests. The snags
   occurred because we did not take 
   distributivity into account.
   
   Does the process always terminate?
   We know already that pat-match
   always terminates if given sensible
   rules. Are these rules sensible? 
   Yes, because no rule undoes another
   rule. In particular, the antirule
   x * n = n * x 
   is liable to infinite recursion, 
   since x could match a number.
|#

;;; now, we extend the simplification
;;; rules to cover exponentiation 
;;; and logarithms, as well as trig functions

(setf *simplification-rules*
  (append *simplification-rules*
    (mapcar #'simp-rule
      '((log 1 = 0)
        (log 0 = undefined)
        (log e = 1)
        (sin 0 = 0)
        (sin pi = 0)
        (cos 0 = 1)
        (cos pi = -1)
        (sin (pi / 2) = 1)
        (cos (pi / 2) = 0)
        (log (e ^ x) = x)
        (e ^ (log x) = x)
        ((x ^ y) * (x ^ z) = x ^ (y + z))
        ((x ^ y) / (x ^ z) = x ^ (y - z))
        (log x + log y = log (x * y))
        (log x - log y = log (x / y))
        ((sin x) ^ 2 + (cos x) ^ 2 = 1)))))
        

;;; To extend the system to differentiation
;;; and integration, we need first to come
;;; up with a notation for the operations.
;;; For differentiation, we will employ
;;; (d y x), where y is the function being
;;; differentiated in terms of x. Note that
;;; d will not be mistaken for a variable,
;;; since variables are the single letters
;;; between m and z. The user can enter
;;; (d y / d x), which is Newton's 
;;; notation. For integration, we will
;;; replace the integration sign ∫ with 
;;; ``int''. Thus (int y x) will be 
;;; the notation for ∫ y dx. The user can
;;; write (int y d / x). First, we update
;;; the infix rules, making sure these new
;;; forms appear before division forms.

;;; The new *infix->prefix-rules* has been
;;; moved right under the original
;;; definition.

;;; Next, update the simplification rules 
;;; to include differentiation--we consider
;;; integration later.

;;; The final rule handles cases where the
;;; expression u is not a function of x.
;;; Notice that this will automatically
;;; give us the correct answer when u
;;; is some number n, since dn/dx = 0.
;;; Notice that there are two rules for
;;; x ^ y. Strictly speaking, the second
;;; rule should be sufficient. But, since
;;; the first rule is often presented
;;; separately in textbooks, it is retained
;;; here. It might be a little quicker to
;;; have both rules when we have x ^ (some
;;; number).
           
(setf *simplification-rules*
  (append *simplification-rules*
    (mapcar #'simp-rule
      '((d x / d x = 1)
        (d (u + v) / d x =
          (d u / d x) + (d v / d x))
        (d (u - v) / d x =
          (d u / d x) - (d v / d x))
        (d (- u) / d x =
          - (d u / d x))
        (d (u * v) / d x =
          u * (d v / d x) + v * (d u / d x))
        (d (u / v) / d x =
          v * (d u / d x) - 
          u * (d v / d x) /
          v ^ 2)
        (d (u ^ ?n) / d x =
           n * u ^ (n - 1) * (d u / d x))
        (d (u ^ v) / d x =
           v * u ^ (v - 1) * (d u / d x) +
           u ^ v * (log u) * (d v / d x))
        (d (log u) / d x =
           (d u / d x) / u)
        (d (sin u) / d x =
           (cos u) * (d u / d x))
        (d (cos u) / d x =
           - (sin u) * (d u / d x))
        (d (e ^ u) / d x =
           (e ^ u) * (d u / d x))
        (d u / d x = 0)))))

;;; It works rather well with differentiation.
;;; There is a snag.

;;; (x + y + x + y) -> (X + (Y + (X + Y)))
;;; We would prefer (2 * (X + Y))
                                
;;; (3 * x + y + x + 4 * x) ->
;;; ((3 * X) + (Y + (X + (4 * X))))
;;; We would prefer ((8 * X) + Y)
                                
;;; A solution will come in a later chapter.

;;; Integration poses a much thornier
;;; problem then differentiation. There 
;;; is an algorithm for computing all 
;;; derivatives. Integration, however
;;; is more of an art. It draws on several
;;; techniques. Moreover, some integrals
;;; simply have no closed form. We cannot
;;; solve this issue simply by rule 
;;; manipulation. We can mitigate some
;;; of the problem by trying to apply some
;;; calculus tricks before we apply our 
;;; rules.

(defun simp-fn (op) 
  (get op :simp-fn))

(defun set-simp-fn (op fn)
  (setf (get op :simp-fn) fn))

(defun simplify-exp (exp) ; 2
  "Simplify using a rule or by doing 
  arithmetic, or by using the simp
  function supplied for this operator."
  (cond
   ((simplify-by-fn exp))
   ((rule-based-translator
     exp
     *simplification-rules*
     :rule-if #'exp-lhs
     :rule-then #'exp-rhs
     :action
       #'(lambda (bindings response)
           (simplify
            (sublis bindings response)))
     :var? #'variablep))
   ((evaluable exp) (eval exp))
   (t exp)))

(defun simplify-by-fn (exp)
  "If there is a simplification fn for 
  this exp, and if apply it gives a true
  result, then return the simplified 
  result."
  (let* ((fn (simp-fn (exp-op exp)))
         (result (when fn
                       (funcall fn exp))))
    (when result
          (simplify result))))

;;; A commonly used technique is the
;;; derivative divides technique. 
;;; It is based on:

;;; ∫ f(x) dx = ∫ f(u) du/dx dx
                              
;;; As an example, consider

;;; ∫ x sin(x²) dx
                 
;;; Let u = x², then du/dx = 2x

;;; then ∫ x sin(x²) dx =
;;;      1/2 ∫ sin u du =
;;;      1/2 (- (cos u)) =
;;; - (cos x²) / 2
                 
#| This requires a nondeterministic 
algorithm:

1. Pick a factor of y, calling it f(u)
2. Compute the derivative du/dx
3. Divide y by f(u) * du/dx, calling the
4. quotient k.
5. If k is a constant with respect to x, 
   then the result is k * ∫ f(u) du.
                                   
In our example,
   
u = x²
f(u) = sin x²
du/dx = 2x 
k = 1/2
  
|#

(defun factorize (exp)
  "Return a list of the factors of exp, 
  where each factor is of the form
  (^ y n)"
  (let ((factors nil)
        (constant 1))
    (labels ((fac (x n)
               (cond
                ((numberp x)
                 (setf constant
                       (* constant
                          (expt x n))))
                ((starts-with x '*)
                 (fac (exp-lhs x) n)
                 (fac (exp-rhs x) n))
                ((starts-with x '/)
                 (fac (exp-lhs x) n)
                 (fac (exp-rhs x) (- n)))
                ((and 
                   (starts-with x '-)
                   (length=1
                    (exp-args x)))
                 (setf constant
                       (- constant))
                 (fac (exp-lhs x) n))
                ((and
                    (starts-with x '^)
                    (numberp (exp-rhs x)))
                 (fac (exp-lhs x)
                      (* n (exp-rhs x))))
                (t
                 (let ((factor
                        (find
                         x
                         factors
                         :key #'exp-lhs
                         :test #'equal)))
                   (if factor
                       (incf 
                         (exp-rhs factor)
                         n)
                       (push `(^ ,x ,n)
                              factors)))))))
      (fac exp 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) ,@factors))))))

(defun unfactorize (factors)
  "Convert a list of factors back into
  prefix form."
  (cond
   ((null factors) 1)
   ((length=1 factors) (first factors))
   (t
    `(* ,(first factors) 
        ,(unfactorize (rest factors))))))

(defun length=1 (seq)
  (typecase seq
    (list (and seq (not (rest seq))))
    (vector (= (length seq) 1))))
    

(defun starts-with (seq 
                    item 
                    &key
                    (test #'eql))
  (typecase seq
    (list (funcall test item (first seq)))
    (vector 
      (funcall test item (aref seq 0)))))

(defun divide-factors (numer denom)
  "Divide a list of factors by another,
  producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor 
              (find 
                (exp-lhs d) 
                result
                :key #'exp-lhs
                :test #'equal)))
        (if factor
            (decf (exp-rhs factor)
                  (exp-rhs d))
            (push
             `(^ ,(exp-lhs d)
                 ,(- (exp-rhs d)))
             result))))
    (delete 0 result :key #'exp-rhs)))

(defun free-of (exp var)
  "True if expression has no occurrences
  of var."
  (not (find-anywhere var exp)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree? If so,
  return it."
  (cond
   ((eql item tree) tree)
   ((atom tree) nil)
   ((find-anywhere item (first tree)))
   ((find-anywhere item (rest tree)))))

(defun integrate (exp x)
  ;; First try some trivial examples
  (cond
   ((free-of exp x)
    `(* ,exp ,x)) ; Int c = cx
   ((starts-with exp '+)
    `(+ ,(integrate (exp-lhs exp) x)
        ,(integrate (exp-rhs exp) x)))
   ;; Int (f + g) = Int f + Int g
   ((starts-with exp '-)
    (ecase (length (exp-args exp))
      (1 `(- ,(integrate (exp-lhs exp) x)))
      ;; Int -f = - Int f
      (2 `(- ,(integrate (exp-lhs exp) x)
             ,(integrate (exp-rhs exp) x)))))
      ;; Int (f - g) = Int f - Int g
   ;; Now move the constant factors to the 
   ;; left side of the integral
   ((multiple-value-bind
        (const-factors x-factors)
        (partition-if
         #'(lambda (factor)
             (free-of factor x))
           (factorize exp))
      (simplify
       `(* ,(unfactorize const-factors)
        ;; And try to integrate
           ,(cond
             ((null x-factors) x)
             ((some
               #'(lambda (factor)
                   (deriv-divides
                    factor
                    x-factors
                    x))
                 x-factors))
             ;; other methods here
             (t
              `(int? 
                 ,(unfactorize x-factors)
                 ,x)))))))))

(defun partition-if (pred list)
  "Return two values: elements of list
  that satisfy pred and elements that
  don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values
     (nreverse yes-list)
     (nreverse no-list))))

(defun deriv-divides (factor factors x)
  (assert (starts-with factor '^))
  (let* ((u (exp-lhs factor))
         ;; factor = u^n
         (n (exp-rhs factor))
         (k (divide-factors
             factors
             (factorize
              `(* ,factor
                  ,(deriv u x))))))
    (cond
     ((free-of k x)
      (if (= n -1)
          `(* ,(unfactorize k) (log ,u))
          `(/ (* ,(unfactorize k)
                 (^ ,u ,(+ n 1)))
              ,(+ n 1))))
     ((and (= n 1) (in-integral-table? u))
      (let ((k2 (divide-factors
                 factors
                 (factorize
                  `(* ,u 
                      ,(deriv 
                         (exp-lhs u)
                         x))))))
        (if (free-of k2 x)
            `(* ,(integrate-from-table
                  (exp-op u)
                  (exp-lhs u))
                ,(unfactorize k2))))))))

(defun deriv (y x)
  (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    (let ((rule (infix->prefix i-rule)))
      (setf
       (get (exp-op (exp-lhs (exp-lhs rule)))
            'int)
       rule))))

(defun in-integral-table? (exp)
  (and (expp exp)
       (get (exp-op exp) 'int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst
     arg
     (exp-lhs (exp-lhs (exp-lhs rule)))
     (exp-rhs rule))))

(integration-table
 '((int log(x) d x = x * log(x) - x)
   (int exp(x) d x = exp(x))
   (int sin(x) d x = - cos(x))
   (int cos(x) d x = sin(x))
   (int tan(x) d x = - log(cos(x)))
   (int sinh(x) d x = cosh(x))
   (int cosh(x) d x = sinh(x))
   (int tanh(x) d x = log(cosh(x)))))

(set-simp-fn 'int 
  #'(lambda (exp)
      (integrate (exp-lhs exp)
                 (exp-rhs exp))))

;;; Believe it or not, this worked
;;; as advertised.

#|

Exercise 8.2 [s] Some notations use the 
operator ** instead of ^ to indicate 
exponentiation. Fix infix->prefix so that 
either notation is allowed.
       
|#

#| A change to infix->prefix is not 
required. Simply add a rule to 
*infix->prefix-rules*:

((x? ** y?) (^ x y))

|#

#|
Exercise 8.3 [m] Can the system as is deal 
with imaginary numbers? What are some of 
the difficulties?
|#

#| The system would need an overhaul
to deal with complex numbers. First of all,
Common Lisp's representation of complex
numbers is different than mathematical
and engineering notation. Our rules must
translate between the representations. 
We will need to write a version of each
simplification rule to deal with a 
possible complex answer. We will need
to have another :if variable that 
searches for complex numbers.
         
Another problem is that complex numbers
provide additional solutions to some 
problems: roots of functions, for example.
Do we display all the solutions?
   
|#






  
                 
            

           
                        
  
           
           

    
                                                                                                                                                                                                                                                                                       