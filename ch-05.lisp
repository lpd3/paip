;;;; paip/ch-05.lisp
                   
;;;; Chapter 5: ELIZA: Dialog With a Machine

(in-package :eliza)

(defun simple-equal (x y)
  "Are x and y equal? (don't check 
  inside strings."
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
           (simple-equal 
             (rest x) 
             (rest y)))))
             
(defun pat-match% (pattern input) ; 1
  "Does pattern match input? Any variable
  can match anything."
  (if (variablep pattern)
      t
      (if (or (atom pattern) (atom input))
          (eql pattern input)
          (and 
            (pat-match (first pattern)
                       (first input))
            (pat-match (rest pattern)
                       (rest input))))))

#|

Exercise 5.1 [s] Would it be a good idea to 
replace the complex and form in pat-match 
with the simpler 
     
(every #’pat-match pattern input)?
       
No. EVERY returns T when the shortest
sequence is exhausted. Thus, it would
falsely report success if the pattern
matches a prefix of the input, or vice versa.
For this application, we need the two 
 to be the same length to return T.
        
|#  
   
(defun variablep (x)
  "Is x a variable (a symbol beginning with
  '?' ?"
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

;; Snag: pat-match does not return useful
;; output. We would like the values that 
;; are matched to varibles

(defun pat-match%% (pattern input) ; 2
  "Does pattern match input? Any variable
  can match anything. Now returns useful
  output. Warning: buggy version."
  (if (variablep pattern)
      (list (cons pattern input))
      (if (or (atom pattern) (atom input))
          (eql pattern input)
          (append (pat-match (first pattern)
                             (first input))
                  (pat-match (rest pattern)
                             (rest input))))))

#| This version has five bugs:
   
1.  The EQL may return T, not a list, 
    which will signal on recursion.
2.  EQL returns NIL on failure, but here,
    it will be enclosed in a list as part
    of the pattern.
3.  There is no distinction in return value
    between failure and a success with no
    variables.
4.  The same variable can be bound to more
    than one value.
5.  It always checks all of the pattern and 
    input, even when failure occurs early on.
 |#   
 
;; Success and failure constants

(defconstant +fail+ nil
  "Indicates pat-match failure.")

(defconstant +no-bindings+ '((t . t))
  "Indicates pat-match success, with
  no variables.")
  
;; abstracting away the details

(defun get-binding (var bindings)
  "Find a (<variable> . <value>) pair
  in a list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a
  binding-list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings% (var val bindings) ;1
  "Add a (<var> . <val>) pair to a 
  binding list."
  (cons (cons var val) bindings))
  
;; revised pat-match

(defun pat-match%%% ; 3
    (pattern 
     input 
     &optional (bindings +no-bindings+))
  "Match pattern against input in the 
  context of the bindings. Fixes the 5 bugs
  in the previous version."
  (cond
   ((eq bindings +fail+)
    +fail+)
   ((variablep pattern)
    (match-variable pattern input bindings))
   ((eql pattern input) bindings)
   ((and (consp pattern) (consp input))
    (pat-match
     (rest pattern)
     (rest input)
     (pat-match
      (first pattern)
      (first input)
      bindings)))
   (t +fail+)))

(defun match-variable (var input bindings)
  "Does var match input? Uses (or updates)
  current bindings."
  (let ((binding (get-binding var bindings)))
    (if binding
        (if (equal input (binding-val binding))
            bindings
            +fail+)
        (extend-bindings var input bindings))))
#|
 
(pat-match '(i need a ?x) '(i need a vacation))
-> ((?X . VACATION)(T . T))
   
|#

(defun extend-bindings (var val bindings) ; 2
  "Add a (var . val) binding to a binding
  list. Minor tweak: when a binding is 
  made, remove (t . t) from the list."
  (cons 
    (cons var val)
    (if (eq bindings +no-bindings+)
        nil
        bindings)))

#|

(sublis (pat-match '(i need a ?x)
                    (i need a vacation))
        (what would it mean for you if you 
              got a ?x ?))
-> (WHAT WOULD IT MEAN FOR YOU IF YOU GOT 
         A VACATION ?)
   
(pat-match '(i need a ?x)
            (i really need a ?x))
-> NIL
   

(pat-match '(this is easy) '(this is easy))
-> ((T . T))
   
(pat-match '(?x is ?x) '((2 + 2) is 4))
-> NIL
   
(pat-match '(?x is ?x)
           '((2 + 2) is (2 + 2)))
-> (X . 2 + 2)
   
(pat-match '(?p need . ?x)
           '(i need a long vacation))
-> ((?X A LONG VACATION) (?P . I))
   
|#

;; We want to distinguish segment variables
;; (variables that stand for a series of 
;; symbols) from regular variables (which
;; stand for a single symbol. The 
;; implementation will use the list
;; (?* <var>) to denote a segment var. 
;; The * is a nod to Kleene star notation.

(defun pat-match 
       (pattern input &optional
                (bindings +no-bindings+))
  "Match pattern against input in the 
  context of the bindings. Adds segment
  variable support."
  (cond ((eq bindings +fail+) +fail+)
        ((variablep pattern)
         (match-variable
          pattern
          input
          bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match
          (rest pattern)
          (rest input)
          (pat-match
           (first pattern)
           (first input)
           bindings)))
        (t +fail+)))

(defun segment-pattern-p (pattern)
  "Is this pattern a segment pattern?"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))
       
(defun segment-match% (pattern input bindings
                      &optional (start 0)) ; 1
                        "Match the segment pattern ((?* <var>)) 
  against the input. This initial 
  version does not permit another var
  immediately after a segment pattern."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a
        ;; constant. In other words, a 
        ;; pattern can't have two consecutive
        ;; vars;
        (let ((pos 
                (position 
                  (first pat) 
                  input
                  :start start
                  :test #'equal)))
          (if pos
              (let ((b2
                     (pat-match
                      pat
                      (subseq
                       input
                       pos)
                      bindings)))
                ;; If this match failed, 
                ;; try another, longer one
                ;; If it worked, check that
                ;; the variables match.
                (if (eq b2 +fail+)
                    (segment-match
                     pattern
                     input
                     bindings
                     (1+ pos))
                    (match-variable
                     var
                     (subseq
                      input
                      0
                      pos)
                     b2)))
              +fail+)))))

#|

(pat-match 
 '((?* ?p) need (?* ?x))
 '(mr hulot and i need a vacation))
-> ((?P MR HULOT AND I)
    (?X A VACATION))
   
(pat-match '((?* ?x) is a (?* ?y))
  '(what he is is a fool))
-> ((?X WHAT HE IS) (?Y FOOL))
   
Good!

But look at this:
    
(pat-match '((*? ?x) a b (*? ?x))
           '(1 2 a b a b 1 2 a b))
-> NIL
   
This happens because segment-match
sees that a b comes after 1 2, so ?x is
bound to 1 2. But ?x should be bound to
1 2 a b. The solution is to widen the
binding length on failure.
 
|#

(defun segment-match (pattern input bindings
                      &optional (start 0)) ; 2
  "Match the segment pattern ((?* <var>)) 
  against the input. This initial 
  version does not permit another var
  immediately after a segment pattern. 
  Revision: when there is an initial failure,
  try again with a longer binding."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if pat
        (let ((pos 
                (position 
                  (first pat) 
                  input
                  :start start
                  :test #'equal)))
          (if pos
              (let ((b2
                     (pat-match
                      pat
                      (subseq input pos)
                      (match-variable
                       var
                       (subseq input 0 pos)
                       bindings))))
                (if (eq b2 +fail+)
                    (segment-match
                     pattern
                     input
                     bindings
                     (1+ pos))
                    b2))
              +fail+))
        (match-variable var input bindings))))
           
#|
 
(pat-match '((?* x?) a b (?* x?))
  '(1 2 a b a b 1 2 a b))

-> '((?X 1 2 A B))         
                          
|#

;; Now, some rules

(defun rule-pattern (rule)
  (first rule))

(defun rule-responses (rule)
  (rest rule))
  
#|
A rule:
  
(((?* x?) I want (?* y?))
 (what would it mean if you got ?y ?)
 (why do you want ?y ?)
 (suppose you got ?y soon)) |#

;; Rules can have multiple responses, 
;; chosen at random. Rules are listed
;; in order of decreasing specificity. 
;; This obviates the need to assigning 
;; priority numbers to rules, as was
;; done in the original ELIZA.
                             
(defparameter *eliza-rules*
  '((((?* ?x) hello (?* y*))
     (How do you do. Please state your 
          problem))
    (((?* x?) i want (?* ?y))
     (what would it mean if you got ?y ?)
     (why do you want ?y ?)
     (suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (do you really think it is likely 
         that ?y)
     (do you wish that ?y)
     (what do you think about ?y)
     (really -- if ?y ?))
    (((?* x?) no (?* ?y))
     (why not ?)
     (you are being a bit negative)
     (are you saying "NO" just to be
          negative ?))
    (((?* ?x) i was (?* ?y))
     (were you really ?)
     (perhaps i already knew you were ?y)
     (why do you tell me you were ?y now ?))
    (((?* ?x) i feel (?* ?y))
     (do you often feel ?y ?))
    (((?* ?x) i felt (?* ?y))
     (what other feelings do you have ?))))
     
;; the ELIZA program proper
                          
(defun eliza ()
  "Respond to user input using pattern
  matching rules."
  (loop
    (print 'eliza> *query-io*)
    (write (flatten (use-eliza-rules (read)))
      :stream *query-io* :pretty t 
      :right-margin 40)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform
  the input."
  (some
   #'(lambda (rule)
       (let ((result
               (pat-match 
                 (rule-pattern rule) 
                 input)))
         (when (not (eq result +fail+))
           (sublis
             (switch-viewpoint result)
             (random-elt 
               (rule-responses rule))))))
     *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, etc."
  (sublis
    '((i . you) (you . i) (me . you)
      (am . are))
    words))

(defun flatten (the-list)
  "Append together elements (or lists)
  in the list"
  (mappend #'ensure-list the-list))

#|
Additional functionality of the 
original ELIZA:
 
1. Aliases. e.g., 'mother' and 'father'
   were associated with a 'family'
   pattern. 
   
2. Synonyms: can not = can't, 
   anyone = anybody, etc.
   
3. Selection and prioritized single
   response to an input containing 
   comma-separated patterns
   
4. Memory. If there is no appropriate
   response to an input, the response
   would be TELL ME MORE ABOUT 
   <some previous topic>
   
5. More rules.
   
|#
 
#|

Exercise 5.2 [m] Experiment with this 
version of ELIZA. Show some exchanges where 
it performs well, and some where it fails.
Try to characterize the difference. Which 
failures could be fixed by changing the 
rule set, which by changing the pat-match 
function (and the pattern language it 
defines), and which require a change to the 
eliza program itself? 
      
1. There is no graceful way to
exit. This must be changed in the program,
but it is a very minor change.
    
2. The user inputs that mimic the small
   rule set are convincing. But even the
   simplest unexpected input returns
   NIL, a very bad option. This problem
   resides in the rule set. At the least,
   we need a final default response. 
   Tell me more....for example. 
   It would also be helpful to greatly
   expand the rule set.
   
3. The program has a very rudimentary sense
   of tense. I fear this must be fixed
   in Eliza, and it is no small undertaking.
   
4. Use of figures of speech would throw 
   our Eliza. This is a rule problem, 
   but a formidable one, requiring
   an encyclopedic rule set.
   
5. Humorous language would be impossible
   to distinguish from serious statements.
   This would require an extremely 
   sophisticated change to the program.
   Probably the only fix would be some
   type of training with deep learning
   techniques.
   
6. Once we expand the rules, a series of
   other problems develop. These would
   require the expanded capabilities 
   discussed earlier, particularly 
   synonyms, aliases and memory.
   
Exercise 5.3 [h] Define a new set of rules 
that make ELIZA give stereotypical responses 
to some situation other than the 
doctor–patient relationship. Or, write a set 
of rules in a language other than English. 
Test and debug your new rule set. |#
     
;; I fear that rewriting the rules to
;; suit another language would prove 
;; more difficult than it would at first
;; appear. In particular, we will need to
;; rewrite the change-viewpoint function.
;; The syntax of the target language
;; greatly impacts the difficulty of 
;; this task. To minimize the 
;; changes to the actual structure of the
;; program, we will want our language to
;; use SVO sentence order and be nearly
;; or completely analytic, like English.
;; It should also havs a copula and
;; not have gender.
;; Consider Spanish, which is fusional
;; rather than analytic. It would be 
;; insufficient to change pronouns and 
;; one verb to achieve a change in 
;; viewpoint. Rather, every verb must
;; be changed. Moreover, since Spanish
;; has gender, and the formal singular
;; form of "you" carries gender, 
;; (the indirect object) we would
;; need to somehow divine the sex of the 
;; user to provide appropriate changes.
;; This would require a complete revamping
;; of the program, and in particular, an
;; exhaustive dictionary of verb changes,
;; along with a mechanism to parse
;; reflexive verbs, just to achieve the same
;; functionality.

;; On the other hand, Mandarin, with its
;; analytic grammar (no conjugations), 
;; and lack of grammatical gender would be
;; a relatively easy target. It even has
;; SVO order. Now, Mandarin is a topic--
;; head language. This could present 
;; problems. The major hurtle is the 
;; writing system. But if we stick to 
;; using pinyin, we can proceed with 
;; little extra effort.

;; First, eliza needs to recognize the new
;; change-viewpoint. We'll also give a
;; graceful exit.

(defun chinese-eliza ()
  "Respond to user input using pattern
  matching rules. In Mandarin (Pinyin)."
  (loop
    (print 'eliza> *query-io*)
    (let ((input (read)))
      (cond
       ((listp input)
          (write 
            (flatten 
              (use-chinese-eliza-rules 
                input))
            :stream *query-io* 
            :pretty t 
            :right-margin 40))
       ((member input
          '(q quit exit stop bye good-bye
            good seeya bai bài zàijiàn zài
            tuī))
        (write 
          '(zài jiàn!)
          :stream *query-io*
          :pretty t)
        (return-from chinese-eliza 'bài!))
       (t
        (write
         '(Wǒ bù míngbái : Qǐng jiāng shūrù
           fàng zài guāhào huò shūrù quit)
         :stream *query-io*
         :pretty t
         :right-margin 40))))))

(defun use-chinese-eliza-rules (input)
  "Find some rule with which to transform
  the Pinyin input"
  (some
   #'(lambda (rule)
       (let ((result
               (pat-match 
                 (rule-pattern rule) 
                 input)))
         (when (not (eq result +fail+))
           (sublis
             (switch-chinese-viewpoint 
               result)
             (random-elt 
               (rule-responses rule))))))
     *chinese-eliza-rules*)) 
     
(defun switch-chinese-viewpoint (words)
  "Change wǒ to nǐ and vice versa.
  No need to worry about verb forms
  or pronoun case."
  (sublis
    '((wǒ . nǐ) (nǐ . wǒ))
    words)) 

(defparameter *chinese-eliza-rules*
  '((((?* ?x) nǐ hǎo (?* y*))
     (nǐ hǎo : qǐng shuōmíng nǐ de wèntí))
    (((?* x?) wǒ xiǎng (?* ?y))
     (rúguǒ nǐ dédào ?y : zhè duì nǐ
       yìwèizhe shénme ?)
     (nǐ wèishéme xiǎng yào ?y ?)
     (jiǎshè nǐ hěn kuài dédào ?y))
    (((?* ?x) rúguǒ (?* ?y))
     (Nín rènwéi ?y : Zhēn de yǒu 
       kěnéng ma ?)
     (Nǐ xīwàng ?y ma ?)
     (Nǐ duì ?y yǒu shé me kànfǎ ?)
     (Zhēn de ma ? rúguǒ ?y ?))
    (((?* x?) bù (?* ?y))
     (Wèishéme bù ?)
     (Nǐ yǒudiǎn xiāojí)
     (Nǐ shuō "BÙ" zhǐshì wèile biǎoshì 
       xiāojí ma ?))
    (((?* ?x) Wǒ céng shì (?* ?y))
     (Nǐ zhēn de shì ma ?)
     (Yěxǔ wǒ yǐjīng zhīdào nǐ shì ?y)
     (Nǐ wèishéme xiànzài gàosù wǒ nǐ 
       shì ?y ?))
    (((?* ?x) Wǒ gǎnjué (?* ?y))
     (nǐ gǎnjué ?y : Nǐ jīngcháng zhèyàng 
         zuò ma ?))
    (((?* ?x) Wǒ (?* ?z)
             gǎnshòu (?* ?y))
     (Nín hái yǒu shé me gǎnshòu ?))
    (((?* ?x))
     (Gàosù wǒ gèng duō))))

#|

Exercise 5.4 [s] We mentioned that our 
version of ELIZA cannot handle commas or 
double quote marks in the input. However, 
it seems to handle the apostrophe in both 
input and patterns. Explain.

We are using symbols to accept and return
messages to the user. In general, this is 
an abuse of symbols. A quoted list of 
symbols will make each symbol evaluate
to itself, which is what we want. But 
the period, double quote, comma, 
semicolon, backtick and sharp sign have
syntactic meaning in Lisp. Some of these
syntax characters lose their meaning when
part of a symbol: the single quote or
apostrophe, for example. We generally
want the apostrophe to be part of the 
symbol, but the comma and period, which
are immediately adjacent to a symbol, is
not usually desired to be part of the 
symbol.

Lisp is very picky about double quotes 
(begin and end a string literal), 
commas (unquoting--may appear only within
a semi-quoted expression), semicolons
(comment to the end of the line) and periods 
(indicating that the single
element that follows is the terminus of a 
dotted list). It is even pickier with the
sharp sign, which usually signals a dispatch
macro character. 

In general, if you want to communicate
with a user in a human language (i.e.
beyond raw data), you should really use
strings, not lists of symbols.

Exercise 5.5 [h] Alter the input mechanism 
to handle commas and other punctuation 
characters. Also arrange so that the user 
doesn’t have to type parentheses around the 
whole input expression. (Hint: this can 
only be done using some Lisp functions we 
have not seen yet. Lookat read-line and 
read-from-string.)

Performing this would not be instructive
to me. Suffice it say, that we would 
alter our program to work on strings
rather than lists of symbols. 
read-line returns a line of input in a 
string. If we construct our program
with sequence and string operations,
we need not use read-from-string.

We clean the input string of most 
punctuation marks, and use string-equal, 
which is case-insensitive.

Exercise 5.6 [m] Modify ELIZA to have an 
explicit exit. Also arrange so that the 
output is not printed in parentheses either."

I already did the first part in the Chinese
program. For the second part, the text 
can be returned using

(format *query-io* "~{~A~^ ~}"
                   <response list>)

Exercise 5.7 [m] Add the “memory mechanism” 
discussed previously to ELIZA. Also add some 
way of definining synonyms like “everyone” 
and “everybody.”

The memory system can be implemented
by storing targeted bindings to variables.
We keep a global var *topics*. Every time
a variable is bound which has the potential
of being used in one of the responses, we
store the text to which it refers as a topic.
Then, when a statement is made for 
which there is no response (or at 
random intervals) return 
HMM. TELL ME MORE ABOUT <topic>, where 
<topic> is selected at random from the topic
list. We will need to guard this, in case
an unparseable input occurs before any 
topics have been saved. (TELL ME MORE, e.g.)

The synonyms can be treated similarly to 
the change-viewpoint function. We call
a parse-synonym function, containing 
a hash table of synonym -> known word,
on all inputs before responding.

Exercise 5.8 [h] It turns out that none of 
the rules in the given script uses a 
variable more than once–there is no rule of 
the form (?x… ?x). Write a pattern matcher 
that only adds bindings, never checks 
variables against previous bindings. 
Use the time special form to compare your 
function against the current version.

This is actually quite easy. All we need
to do is revert to an earlier formulation,
in which variable checking was not 
performed. I imagine that the speed
savings would be very modest.

Exercise 5.9 [h] Winston and Horn’s book 
Lisp presents a good pattern-matching 
program. Compare their implementation with 
this one. One difference is that they handle 
the case where the first element of the 
pattern is a segment variable with the 
following code (translated into our 
notation):
(or 
  (pat-match 
    (rest pattern) 
    (rest input) 
    bindings)
  (pat-match 
    pattern 
   (rest input) 
   bindings))
     
This says that a segment variable matches 
either by matching the first element of the 
input, or by matching more than the first 
element. It is much simpler than our 
approach using position, partly because 
they don’t update the binding list. Can you 
change their code to handle bindings, and 
incorporate it into our version of 
pat-match? Is it still simpler? Is it more 
or less efficient?

It would be simpler. It would be less
efficient. The version presented here
requires a recursive application over the
whole input before a decision is made.
Ours captures as quickly as possible, and
only goes longer after failure.

Exercise 5.10 What is wrong with the 
following definition of simple-equal?
              
(defun simple-equal (x y)
“Test if two lists or atoms are equal.”
;; Warning - incorrect
(or (eql x y)
(and (listp x) (listp y)
(simple-equal (first x) (first y))
(simple-equal (rest x) (rest y)))))

The program never terminates if two 
equal lists are given. This is because
the final element of a proper list is nil.
Nil is itself a list. Its first item
is nil and the rest of it is nil.
We would never reach the end of our 
check. Instead of listp, we should use 
consp. This returns T for a list with
elements and NIL (i.e. false) for an 
empty list (i.e. NIL).

Exercise 5.12 [m] Weigh the advantagesof 
making pat-match return multiple values: 
the first would be true for a match and 
false for failure, and the second would
be the binding list.

Well, the implementation would be a tiny
bit more complicated. We would need to 
keep track of both a success variable
and the binding list. But it is a cleaner
solution.

|#






     



                                   
  
   




     
     

  
            

    
  

                          



      