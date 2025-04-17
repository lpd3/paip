;;;; paip/ch-4.lisp
                  
;;;; Chapter 4: The General Problem Solver

(in-package :general-problem-solver)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not
                 &allow-other-keys)
  "Find all elements in the sequence that 
  match item, according to the kwargs."
  (if test-not
      (apply 
        #'remove 
        item 
        sequence
        :test-not (complement test-not)
        keyword-args)
      (apply
       #'remove
       item
       sequence
       :test (complement test)
       keyword-args)))

(defvar *state* nil
  "The current state: a list of conditions")

(defvar *ops* nil
  "A list of available operators.")

(defstruct op 
  "an operation"
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))
  
(defun appropriatep% (goal op) ; 1
  "An op is appropriate to a goal if the
  goal is in the op's add-list"
  (member goal (op-add-list op)))

(defun apply-op% (op) ; 1
  "Print a message and update *state* if op
  is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf 
      *state* 
      (set-difference 
        *state*
        (op-del-list op)))
    (setf
      *state*
      (union
        *state*
        (op-add-list op)))))
    

(defun achieve% (goal) ; 1
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it
  that is applicable."
  (or
   (member goal *state*)
   (some
    #'apply-op
    (find-all 
      goal 
      *ops* 
      :test #'appropriatep))))

(defun gps% (*state* goals *ops*) ; 1
  "General Problem Solver: achieve all
  goals using *ops*"
  (when
    (every #'achieve goals)
    'solved))
#|
(make-op 
  :action 'drive-son-to-school
  :preconds '(son-at-home car-works)
  :add-list '(son-at-school)
  :del-list '(son-at-home)) |#
                             
(defparameter *school-ops*
  (list
   (make-op
    :action 'drive-son-to-school
    :preconds '(son-at-home car-works)
    :add-list '(son-at-school)
    :del-list '(son-at-home))
   (make-op
    :action 'shop-installs-battery
    :preconds '(car-needs-battery
                shop-knows-problem
                shop-has-money)
    :add-list '(car-works))
   (make-op 
     :action 'tell-shop-problem
     :preconds '(in-communication-with-shop)
     :add-list '(shop-knows-problem))
   (make-op
    :action 'telephone-shop
    :preconds '(know-phone-number)
    :add-list '(in-communication-with-shop))
   (make-op
    :action 'look-up-number
    :preconds '(have-phone-book)
    :add-list '(know-phone-number))
   (make-op
    :action 'give-shop-money
    :preconds '(have-money)
    :add-list '(shop-has-money)
    :del-list '(have-money))))

#|

(gps '(son-at-home car-needs-battery 
       have-money have-phone-book)
     '(son-at-school)
     *school-ops*)

printout-> (EXECUTING LOOK-UP-NUMBER)
printout-> (EXECUTING TELEPHONE-SHOP)
printout-> (EXECUTING TELL-SHOP-PROBLEM)
printout-> (EXECUTING GIVE-SHOP-MONEY)
printout-> (EXECUTING SHOP-INSTALLS BATTERY)
printout-> (EXECUTING DRIVE-SON-TO-SCHOOL)
-> SOLVED
        
(gps '(son-at-home car-needs-battery
       have-money)
     '(son-at-school)
     *school-ops*)

-> NIL
     
(gps '(son-at-home car-works)
     '(son-at-school)
     *school-ops*)

printout-> (EXECUTING DRIVE-SON-TO-SCHOOL)
-> SOLVED
        
|#

#| Problems
   
1. Running around the block: what if our
   goal involves no net change of state?
   We come back to this later.
   
2. Clobbered sibling goal: what if we have
   more than one final goal and the steps
   required to
   meet one goal undo an alteady-met
   goal? What if we attempt to achieve the
   already-met goal first? The program
   will incorrectly announce total success.
   There is an easy solution, however. |#
   
(defun gps%% (*state* goals *ops*) ; 2
  "General Problem Solver: achieve all
  goals using *ops*. Fixed the ``clobbered
  sibling goal'' problem."
  (and
   (every #'achieve goals)
   (subsetp goals *state*)))

#| Problems, continued
   
3. Leaping before you look: We have more
   than one goal, we go through all the 
   work to solve the first goal before
   realizing the second goal is impossible.
   One solution is to replace the global
   *state* variable with separate, distinct,
   local state variables.
   
4. Recursive subgoal: what if we have a 
   goal, and one of the steps required to
   solve the goal requires that first tge
   main goal be met? We go into an 
   infinite loop. This can also be 
   solved by employing local state
   variables.
   
5. Lack of intermediate information: 
   If the attempt to meet a goal fails, 
   the program returns NIL without 
   telling us why it failed. We can begin
   to solve this by employing structured
   debugging. |#
               
(defvar *dbg-ids* nil
  "Identifiers used by dbg.")

(defun dbg% (id format-string &rest args) ; 1
  "Print debugging info if (debug* <id>)
  has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply
     #'format
     *debug-io*
     format-string
     args)))

(defun debug* (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids*
        (union ids *dbg-ids*)))

(defun undebug* (&rest ids)
  "Stop dbg on the ids. When no ids, 
  stop debugging altogether."
  (setf *dbg-ids* 
        (if (null ids)
            nil
            (set-difference
             *dbg-ids* ids))))

(defun dbg-indent% (id indent format-string &rest args)
  "Print indented debugging info if 
  (debug* <id>) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent)
      (princ " " *debug-io*))
    (apply
     #'format
     *debug-io*
     format-string
     args)))

(setf (symbol-function 'find-all-if)
      #'remove-if-not)

#| An improved implementation follows. 
Instead of printing '(EXECUTING ...), we
will return the state, which includes
executing messages, and will resolve the
running around the block problem |#
                                  
(defun starts-with (lst x)
  "Is this a list whose first element is x?"
  (and
   (consp lst)
   (eql (first lst) x)))
        
(defun executingp (x)
  "Is x of the form (executing...) ?"
  (starts-with x 'executing))

(defun convert-op (op)
  "Make op conform to the (executing op)
  convention."
  (unless
    (some
     #'executingp
     (op-add-list op))
    (push (list 'executing (op-action op))
          (op-add-list op)))
  op)
          
(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the 
  (executing op) convention."
  (convert-op
   (make-op
    :action action
    :preconds preconds
    :add-list add-list
    :del-list del-list)))

;; Instead of starting over, we use Lisp
;; to alter a pre-existing implementation.
;; Example of exploratory programming.

(mapc #'convert-op *school-ops*)
      
;; *state* remains the same
;; *ops* remains the same
;; the struct definition op remains the same

(defun member-equal (item lst)
  (member item lst :test #'equal))

(defun appropriatep (goal op) ; 2
  "An op is appropriate to a goal if it is 
  in its add-list. 2nd implementation."
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack) ; 2
  "Return a new, transformed state if op
  is applicable."
  (dbg-indent 
    :gps 
    (length goal-stack)
    "Consider: ~A" 
    (op-action op))
  (let ((state2 
          (achieve-all state 
            (op-preconds op)
            (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent 
        :gps
        (length goal-stack)
        "Action: ~A" 
        (op-action op))
      (append
       (remove-if
        #'(lambda (x)
            (member-equal x (op-del-list op)))
          state2)
       (op-add-list op)))))

(defun achieve%% (state goal goal-stack) ; 2
  "A goal is achieved if it already holds,
   or if there is an appropriate op for it
   that is applicable. Second implementation."
  (dbg-indent 
    :gps 
    (length goal-stack)
    "Goal: ~A"
    goal)
  (cond
   ((member-equal goal state) state)
   ((member-equal goal goal-stack) nil)
   (t
    (some
     #'(lambda (op)
         (apply-op
          state
          goal
          op
          goal-stack))
       (find-all
        goal
        *ops*
        :test #'appropriatep)))))

(defun achieve-all% (state goals goal-stack)
  ;1
  "Achieve each goal, and make sure they
  still hold at the end."
  (let ((current-state state))
    (if (and
         (every
          #'(lambda (g)
              (setf
               current-state
               (achieve
                current-state
                g
                goal-stack)))
            goals)
         (subsetp
          goals
          current-state
          :test #'equal))
        current-state)))
        
(defun gps%%% (state goals &optional (*ops* *ops*)) ; 3
  "General Problem Solver: achieve all
  goals using *ops*. New implementation."
  (remove-if
    #'atom
    (achieve-all
     (cons '(start) state)
     goals
     nil)))

(defun use (oplist)
  "Use oplist as the default set of 
  operators."
  ;; Return something useful, but not 
  ;; too verbose
  (length (setf *ops* oplist)))

(push (op 'ask-phone-number
          :preconds '(in-communication-with-shop)
          :add-list '(know-phone-number))
      *school-ops*)

#| 
  
(use *school-ops*)

-> 7
  
(gps '(son-at-home car-needs-battery 
       have-money have-phone-book)
     '(son-at-school))
     
-> ((START) 
    (EXECUTING LOOK-UP-NUMBER)
    (EXECUTING TELEPHONE-SHOP)
    (EXECUTING TELL-SHOP-PROBLEM)
    (EXECUTING GIVE-SHOP-MONEY)
    (EXECUTING SHOP-INSTALLS-BATTERY)
    (EXECUTING DRIVE-SON-TO-SCHOOL))
   
(debug* :gps)

-> (:GPS)
   
(gps '(son-at-home car-needs-battery
       have-money have-phone-book)
     '(son-at-school))

[printout to *debug-io*]
          
-> Goal: SON-AT-SCHOOL
-> Consider: DRIVE-SON-TO-SCHOOL
->  Goal: SON-AT-HOME
->  Goal: CAR-WORKS
->  Consider: SHOP-INSTALLS-BATTERY
->   Goal: CAR-NEEDS-BATTERY
->   Goal: SHOP-KNOWS-PROBLEM
->   Consider: TELL-SHOP-PROBLEM
->    Goal: IN-COMMUNICATION-WITH-SHOP
->     Consider: TELEPHONE-SHOP
->      Goal: KNOW-PHONE-NUMBER
->      Consider: ASK-PHONE-NUMBER
->       Goal: IN-COMMUNICATION-WITH-SHOP
->      Consider: LOOK-UP-NUMBER
->       Goal: HAVE-PHONE-BOOK
->      Action: LOOK-UP-NUMBER
->     Action: TELEPHONE-SHOP
->    Action: TELL-SHOP-PROBLEM
->    Goal: SHOP-HAS-MONEY
->    Consider: GIVE-SHOP-MONEY
->     Goal: HAVE-MONEY
->    Action: GIVE-SHOP-MONEY
->   Action: SHOP-INSTALLS-BATTERY
->  Action: DRIVE-SON-TO-SCHOOL

[Return value at *standard-output* same 
as before]

(undebug*)
-> NIL
(gps '(son-at-school car-works) '(son-at-school))
-> ((START)
    (EXECUTING DRIVE-SON-TO-SCHOOL)) 

(gps '(son-at-home car-needs-battery
       have-money have-phone-book)
     '(son-at-school have-money))
-> NIL
(gps '(son-at-home car-needs-battery
       have-money)
     '(son-at-school))
-> NIL
(gps '(son-at-school) '(son-at-school))

-> ((START))

All 5 problems have been addressed

|#

;;; changing the domain

(defparameter *banana-ops*
  (list
    (op 'climb-on-chair
        :preconds
        '(chair-at-middle-room
          at-middle-room
          on-floor)
        :add-list '(at-bananas on-chair)
        :del-list '(at-middle-room
                    on-floor))
    (op 'push-chair-from-door-to-middle-room
        :preconds '(chair-at-door
                    at-door)
        :add-list '(chair-at-middle-room
                    at-middle-room)
        :del-list '(chair-at-door
                    at-door))
    (op 'walk-from-door-to-middle-room
        :preconds '(at-door on-floor)
        :add-list '(at-middle-room)
        :del-list '(at-door))
    (op 'grasp-bananas
        :preconds '(at-bananas empty-handed)
        :add-list '(has-bananas)
        :del-list '(empty-handed))
    (op 'drop-ball
        :preconds '(has-ball)
        :add-list '(empty-handed)
        :del-list '(has-ball))
    (op 'eat-bananas
        :preconds '(has-bananas)
        :add-list '(empty-handed not-hungry)
        :del-list '(has-bananas 'hungry))))

#|

(use *banana-ops*)
-> 6
(gps '(at-door on-floor has-ball 
       hungry chair-at-door) '(not-hungry))
-> ((START) 
    (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
    (EXECUTING CLIMB-ON-CHAIR)
    (EXECUTING DROP-BALL)
    (EXECUTING GRASP-BANANAS)
    (EXECUTING EAT-BANANAS)) |#

;; a third domain, getting through
;; a maze, each room numbered. For this
;; we create a function to make the ops.

(defun make-maze-op (here there)
  "Make an operator to move between two
   places."
   (op `(move from ,here to ,there)
       :preconds `((at ,here))
       :add-list `((at ,there))
       :del-list `((at ,here))))

(defun make-maze-ops (pair)
  "Make maze ops in both directions."
  (list
    (make-maze-op (first pair) (second pair))
    (make-maze-op (second pair) (first pair))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
    '((1 2) (2 3) (3 4) (4 9) (9 14)
      (9 8) (8 7) (7 12) (12 13) (12 11)
      (11 6) (11 16) (16 17) (17 22)
      (21 22) (22 23) (23 18) (23 24)
      (24 19) (19 20) (20 15) (15 10)
      (10 5) (20 25))))

;; It gets us through the maze, but there
;; is a problem: sometimes (at <some number>)
;; makes it into the results. The problem
;; is that some of the elements of 
;; state lists remain in the final result.
;; The problem was caused by gps, which 
;; searches for items to clean out of 
;; the state lists. It seaeches for atoms.
;; This is a pun. It is a bad practice. 
;; We should say what we mean

(defun gps (state goals &optional (*ops* *ops*))
  ;; 4
  "General Problem Solver: from state, 
  achieve goals using *ops*. 4th version.
  Does not use a pun."
  (find-all-if
   #'actionp
   (achieve-all
    (cons '(start) state)
    goals
    nil)))

(defun actionp (x)
  "Is x something that is (start) or
  (executing ...)?"
  (or (equal x '(start))
      (executingp x)))

;; Our 2nd implementation has an added
;; advantage: it retains all the steps 
;; acted on. We can put this to good use.

(defun find-path (start end)
  "Search a maze for a path from start
  to end."
  (let ((results
         (gps `((at ,start)) `((at ,end)))))
    (when results
      (cons 
        start 
        (mapcar 
          #'destination
          (remove 
            '(start)
            results 
            :test #'equal))))))

(defun destination (action)
  "Find the y in 
  (executing (move from x to y))"
  (fifth (second action)))

#|

(find-path 1 25)
-> (1 2 3 4 9 8 7 12 11 16 17 22 23 24 19 20 25)
|#
;;; A fourth domain: block world
                               
(defun make-block-ops (blocks)
  (let (ops)
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal a c) 
                        (equal b c))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move a from b to c."
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a)
                  (space on ,c)
                  (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))

(defmacro debug-show ((&optional program
                       (stream *standard-output*))
                      expr)
  (when program
    (debug* program))
  `(let ((*debug-io* ,stream))
     ,expr))

#|

Problem: Sometimes, the order in which
final goals are presented changes the result.
      
E.g. With initial state
     
a on b, b on c, c on table, space on a, 
space on table,
      
if our goals are
   
a on table, b on a, c on b, 
  
the program successfully executes.
    
But if the goals are
    
c on b, b on a, a on table,
  
the program fails. This is because in the
attempt to solve the latter, a sibling 
goal is clobbered. The program simply 
gives up rather than backtrack to find
a solution.
  
One fix for this problem is to 
reverse the goals and try again when there
is failure. This would solve a subset of 
the problem, particularly those cases
where there are relatively few interactions
between goals.
        
here is the change that would accomplish 
this: |#

(defun achieve-all (state goals goal-stack)
  ;; 2
  (some
    #'(lambda (goals)
        (achieve-each state goals goal-stack))
      (orderings goals)))

(defun achieve-each (state goals goal-stack)
  (let ((current-state state))
    (if 
      (and 
        (every 
          #'(lambda (g)
              (setf 
                current-state
                (achieve 
                  current-state 
                  g 
                  goal-stack)))
          goals)
        (subsetp 
          goals
          current-state
          :test #'equal))
      current-state)))

(defun orderings (lst)
  (if (> (length lst) 1)
      (list lst (reverse lst))
      (list lst)))


#|       
Another solution would be to 
search as much of the entire solution space as 
necessary. This brute force approach 
very quickly becomes impractical as the size
of the task grows.
   
Another problem: a found solution is not 
guaranteed to be the most efficient one.
            
An example
   
C
A   B   ____
    
Goal:

A   B    C
    
The obvious solution is simply to move C to
the space on the table. But if the
state facts are listed in a certain 
order, gps first moves
C to B then puts C on the table.
  
One fix is to do a depth-first search
of the solution space.
   
Another fix is to sort the solutions
in order of increasing number of unmet
prerequisites.

|#

(defun achieve (state goal goal-stack) ; 3
  "A goal is achieved if it already holds,
   or if there is an appropriate op for it 
   that is applicable. Version 3: juggles
   the order of goals ops so that the
   applicable ones with the fewest number 
   of as yet unmet prerequisites are first.
   Yields more efficient solutions."
  (dbg-indent :gps (length goal-stack)
    "Goal: ~A" goal)
  (cond
   ((member-equal goal state) state)
   ((member-equal goal goal-stack) nil)
   (t
    (some
     #'(lambda (op)
         (apply-op
          state
          goal
          op
          goal-stack))
       (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators,
  sorted by the number of unfilled 
  preconditions."
  (sort
   (copy-list
    (find-all
     goal
     *ops*
     :test #'appropriatep))
   #'<
   :key #'(lambda (op)
            (count-if 
              #'(lambda (precond)
                  (not
                   (member-equal
                    precond
                    state)))
               (op-preconds op)))))

;; Another Jam: some collections of goals,
;; while mutually possible, are nevertheless
;; impossible to solve with our implementation,
;; regardless of the ordering of ops.
                                    
#| Consider:
   
Initial state:
      C
B __  A       
  
Goal State:
     
A
B
C

When we solve by hand, we get:
     
C->Table
B->C
A->B
solved

When we give this to the GPS, it fails. 
Why?
There is no successful path through the
problem that does not require that we
temporarily undo a met goal to achieve
an intermediate goal. GPS is written to
abandon a computational path if it 
clobbers a sister goal. Since all successful
paths in this problem do so, GPS fails.
This issue is known as the Sussman anomaly.
     
Analyzing the second implementation:
          
We are left with some more snags that 
we didnt consider before:
   
1. Not looking after you don't leap:
If a particular op undoes a sister goal
or presents a recursive path, our GPS
fails, even if another op could be 
successfully employed instead.
             
Consider our school ops. We push the following
op to *school-ops*

(op 'taxi-son-to-school
    :preconds '(son-at-home have-money)
    :add-list '(son-at-school)
    :del-list '(son-at-home have-money))

Then, we run the following:
      
(gps '(son-at-home have-money car-works)
     '(son-at-school have-money))

GPS fails. Why? It attempts 'taxi-son-to-school
and finds that there is no way to 
taxi the child to school and have money 
left. This is true. But why not drive the
kid to school? After all, the car works.
Because the taxi ride was considered first
and abandoned, without looking for other
possibilities. 
              
The issue is caused by the SOME operator
in achieve-all.
   
One fix is to explore other possible paths
beyond the first apparently appropriate
one. Prolog uses such a strategy.
     
Another solution is to guard goals that 
must not be removed by an op. Warplan
uses such a strategy.
     
2. Lack of descriptive power
   
There are many problem types that are
unsolvable with our GPS. Often these
take the form of constraint propagation.
Consider a chess game. The final goal
does not rely on specific board locations,
but rather on the relationships between
pieces and locations. Consider our 
maze problem. It would be better to supply
a graph rather than edges. Such 
problems require variables. There is also
the issue of timing, which is usually
important in the real world, but not 
mentioned here. Many problems involve
some kind of min or max (say, of cost
or value). This was brought
up earlier in the block moving ops, 
in regards to the efficiency of solutions.
Finally, sometimes a partial solution is
useful. Our program can succeed or fail.
There is nothing in between.
                           
3. Perfect Information: Our GPS represents
a world with perfect info. There are no
unknowns in the raw data. There is no
room for probability, chance, or random
events. But all of these are important 
factors in real-world problems.
        
4. Interacting Goals. In real life, people
   have many goals at once. Some are in the
   background. Goals emerge and recede. We
   find our own goals. None of this 
   is in our program. The goals here are 
   largely blind to side effects and 
   completely blind to cost/benefit 
   analysis. |#
   
#|

Exercise 4.1 [m] It is possible to implement 
dbg using a single call to format. Can you 
figure out the format directives to do this?
|#

(defun dbg (id format-control &rest format-args)
  (format t "~@[~&~?~]"
           (and
            (member id *dbg-ids*)
            format-control)
           format-args))

(defun dbg-indent 
    (id indent format-control 
     &rest format-args)
  (when (member id *dbg-ids*)
    (format
      t 
      "~&~V@T~?"
      indent               
      format-control 
      format-args)))
              
  

#| 
Exercise 4.2 [m] Write a function that 
generates all permutations of its input. |#
          
(defun heap-perms (&rest items)
  "Non-recursive variant of heap's 
  algorithm. Accepts an arbitrary
  number of items. Puts them in an 
  array for efficiency. Returns a list
  of all the permutations of the items,
  each permutation in an array."
  (let* ((item-count (length items))
         (perm (coerce items 'vector))
         (stack-state
          (make-array
           item-count
           :element-type 'fixnum
           :initial-element 0))
         (perms (list perm)))
    (do ((i 1))
        ((= i item-count) (nreverse perms))
      (if (< (aref stack-state i) i)
          (let ((next
                 (copy-seq perm)))
            (if (evenp i)
                (rotatef
                 (aref next 0)
                 (aref next i))
                (rotatef
                 (aref next (aref stack-state i))
                 (aref next i)))
            (push next perms)
            (setf perm next)
            (incf (aref stack-state i))
            (setf i 1))
          (setf (aref stack-state i) 0
                i (1+ i))))))
#| 
  
Exercise 4.3 [h] GPS does not recognize the 
situation where a goal is accidentally 
solved as part of achieving another goal. 
Consider the goal of eating dessert. Assume 
that there are two operators available: 
eating ice cream (which requires having the 
ice cream) and eating cake (which requires 
having the cake). Assume that we can buy a 
cake, and that the bakery has a deal where 
it gives out free ice cream to each 
customer who purchases and eats a cake. 
(1) Design a list of operators to represent 
this situation. 
(2) Give gps the goal of eating dessert. 
Show that, with the right list of operators, 
gps will decide to eat ice cream, then 
decide to buy and eat the cake in order to 
get the free ice cream, and then go ahead 
and eat the ice cream, even though the goal 
of eating dessert has already been achieved 
by eating the cake. 
(3) Fix gps so that it does not manifest 
this problem. |#  
     
(defparameter *cake-ops*
  (list
   (op 'eat-ice-cream
       :preconds '(hungry-for-dessert
                   have-ice-cream)
       :add-list '(not-hungry-for-dessert)
       :del-list '(hungry-for-dessert
                   have-ice-cream))
   (op 'eat-cake
       :preconds '(hungry-for-dessert
                   have-cake)
       :add-list '(not-hungry-for-dessert
                   have-ice-cream)
       :del-list '(hungry-for-dessert
                   have-cake))))  
                                 
;; I cannot get these ops to misbehave.
