#| paip symbols

ch-02   ; A Simple Lisp Program
package: :02-simple-program

*bigger-grammar*
*gramática-sencilla-española*
*grammar*
*simple-grammar*
adj
adj*
article
combine-all
cross-product
generate
generate-all
generate-tree
noun
noun-phrase
one-of
pp
pp*
prep
random-elt
rewrites
rule-lhs
rule-rhs
sentence
verb
verb-phrase

ch-03 ; An Overview of Lisp
package: :lisp-overview

dot
dot2
proper-list-p
red-len

ch-04 ; The General Problem Solver
package: :general-problem-solver

*banana-ops*
*cake-ops*
*dbg-ids*
*maze-ops*
*ops*
*school-ops*
*state*
achueve
achieve-all
achieve-each
actionp
apply-op
appropriate-ops
appropriatep
convert-op
dbg
dbg-indent
debug*
destination
executingp
find-all
find-all-if
find-path
gps
heap-perms
make-block-ops
make-maze-op
make-maze-ops
member-equal
move-ons
move-op
op
orderings
starts-with
undebug*
use

ch-05 ; ELIZA: Dialog With a Machine
package: :eliza

+fail+
+no-bindings+
*chinese-eliza-rules*
*eliza-rules*
binding-val
chinese-eliza
eliza
extend-bindings
flatten
get-binding
lookup
match-variable
pat-match
rule-pattern
rule-responses
segment-match
segment-pattern-p
simple-equal
switch-chinese-viewpoint
switch-viewpoint
use-chinese-eliza-rules
use-eliza-rules
variablep

ch-06-tools ; Building Software Tools
package: :tools

?[
?]
??
?+
?*
?and
?if
?is
?not
?or
+earth-diameter+
+fail+
+no-bindings+
*cities*
a*-search
air-distance
beam-search
best-first-search
better-path
binary-tree
binding-val
binding-var
breadth-first-search
city
deg->radians
depth-first-search
diff
distance
expand-pat-match-abbrev
extend-bindings
find-all-if
find-path
finite-binary-tree
first-match-pos
get-binding
graph-search
insert-path
interactive-interpreter
is
iter-wide-search
literal-match
lookup
make-binding
match-and
match-if
match-is
match-or
match-variable
map-path
neighbors
new-states
next2
pat-match
pat-match-abbrev
path
path-saver
path-states
prepend
price-is-right
print-path
prompt-generator
rm-tmps
rule-based-translator
search-all
segment-match
segment-match?
segment-match+
segment-match-fn
segment-matcher
segment-pattern-p
show-city-path
single-match-fn
single-matcher
single-pattern-p
sorter
tree-search
trip
variablep
xyz-coords

ch-07 ; STUDENT: Solving Algebra Word Problems
package: :student

+operators-and-inverses+
*student-rules*
binary-exp-p
commutativep
create-list-of-equations
exp
exp-args
expp
in-exp
inverse-op
isolate
make-variable
one-unknown
no-unknown
noise-word-p
prefix->infix
print-equations
rule
solve
solve-arithmetic
solve-equations
student
translate-pair
translate-to-expression
unknownp

ch-08; Symbolic Mathematics: A Simplification Program
package: :symbolic

^
*infix->prefix-rules*
*simplification-rules*
binary-exp-p
clear-plists
clear-props
d
deriv
deriv-divides
divide-factors
e
evaluable
exp
exp-args
exp-lhs
exp-op
exp-rhs
expp
factorize
find-anywhere
free-of
in-integral-table?
infix->prefix
int
integrate
integrate-from-table
integration-table
length=1
not-number-p
partition-if
prefix->infix
rule
rule-pattern
rule-response
set-simp-fn
simp
simp-fn
simp-rule
simplifier
simplify
simplify-by-fn
simplify-exp
starts-with
undefined
unfactorize
variablep
x
x+
y
y+

ch-09-efficiency ;  Efficiency Issues
package: :efficiency

*answers*
*empty-pipe*
*primes*
*profile-call-stack*
*profiled-functions*
*test-data*
append-pipes
assert-equal
build-cases
build-code
compile-rule
defrule
delay
fib
enumerate
fast-time->seconds
fast-time-difference
filter
force
get-fast-time
head
inc-profile-time
integers
make-pipe
map-pipe
mappend-pipe
memo
memoize
one-of
pipe-elt
populate-primes
profile
profile-count
profile-enter
profile-exit
profile-report
profile-time
profile1
profiled-fn
random-elt
rule-lhs
rule-rhs
sieve
tail
test-it
unprofile
unprofile1
with-profiling

ch-09a  ; applying the lessons of ch 9 to 
        the ch 8 simplification program
package: :efficiency-simplify

*answers*
*rules-for*
*test-data*
assert-equal
index-rules
main-op
rules-for
simplify
simplify-exp
test-it

ch-9b ; Compiling the Simplify Program
package: :compile-simplify

*bindings*
build-exp
combine-rules
compile-all-rules-indexed
compile-args
compile-exp
compile-indexed-rule
compile-rule
compile-rule-set
concat-symbol
exp-lhs
exp-op
exp-rhs
expp
last1
make-exp
matching-ifs
new-symbol
op?
simplify-exp
variablep

ch-10-more-efficiency ; Low-Level Efficiency Issues
package :more-efficiency

+trie-deleted+
*uniq-atom-table*
*uniq-cons-table*
defresource
deftable
defun*
delete-trie
dequeue
efficient-pat-match
efficient-pat-match-2
empty-queue-p
enqueue
ensure-atom
find-trie
flatten1
flatten2
follow-arc
front
get-trie
key
make-queue
match-var
match-var-2
pat-match-1
pat-match-1-2
put-trie
queue-contents
queue-nconc
remq
reuse-cons
square
sum-squares
sum-squares-vec-of-ints
tconc
trie
uappend
ucons
ulist
unique
unique-cons
variablep
with-gensyms
with-resource
            
ch-11 ;; Logic Programming
package: :logic
      
+fail+
+no-bindings+
*db-predicates*
*occurs-check*
<-
?-
add-clause
binding-val
clause-body
clause-head
clear-db
clear-predicate
continuep
ensure-list
extend-bindings
fact
get-binding
get-clauses
lookup
match-variable
occurs-check
predicate
prove
prove-all
rename-variables
replace-?-vars
rule
show-prolog-solutions
show-prolog-vars
subst-bindings
top-level-prove
unifier
unify
unify-variable
unique-find-anywhere
variablep
variables-in
           
ch-12 ;; compiling logic programming
package: :compile-logic
      
+unbound+
*db-predicates*
*trail*
*uncompiled*
*var-counter*
<-
?-
add-clause
anonymous-variables-in
args
bind-unbound-vars
bound-p
clauses-wuth-arity
compile-arg
compile-body
compile-call
compile-clause
compile-predicate
compile-unify
def-prolog-compiler-macro
deref
deref-exp
find-if-anywhere
has-variable-p
ignore*
make-=
make-anonymous
make-parameters
maybe-add-undo-bindings
prolog-compile
prolog-compile-symbols
prolog-compiler-macro
relation-arity
run-prolog
set-binding!
show-prolog-vars
top-level-prove
undo-bindings!
unify!
var

ch-13 Object-Oriented Programming
package: :object

+fail+
account
audited-account
beam-problem
best-problem
bfs-problem
binary-tree-eql-best-beam-problem
binary-tree-eql-bfs-problem
binary-tree-problem
cost-fn
define-class
dfs-problem
ensure-generic-fn
eql-problem
generic-fn-p     
get-method
goal-p
limited-account
make-clause
new-account
problem
problem-combiner
problem-successors
searcher
send
withdraw
       
ch-14 Knowledge Representation and Reasoning
package: :reasoning
      
clear-dtrees
dtree
dtree-atom-fetch
dtree-fetch
dtree-index
fetch
get-dtree
index
lookup-atom
make-empty-nlist
mapc-retrieve
nlist-list
nlist-n
nlist-push
query-bind
retrieve
retrieve-matches
test-index

ch 14a 
package: reasoning-completeness
:+fail+
   :+no-bindings+
   :*db-predicates*
   :*depth-incr*
   :*depth-max*
   :*depth-start*
   :*occurs-check*
   :*search-cutoff*
   :<-
   :?-
   :add-clause
   :binding-val
   :clause-body
   :clause-head
   :clear-db
   :clear-dtrees
   :clear-predicate
   :continuep
   :dtree
   :dtree-atom-fetch
   :dtree-fetch
   :dtree-index
   :ensure-list
   :extend-bindings
   :fetch
   :get-binding
   :get-clauses
   :get-dtree
   :index
   :lookup
   :lookup-atom
   :make-empty-nlist
   :mapc-retrieve
   :match-variable
   :nlist-list
   :nlist-n
   :nlist-push
   :occurs-check
   :predicate
   :prove
   :prove-all
   :query-bind
   :rename-predicates
   :replace-?-vars
   :retrieve
   :retrieve-matches
   :reuse-cons
   :show-prolog-solutions
   :show-prolog-vars
   :subst-bindings
   :test-index
   :top-level-prove
   :unify
   :unify-variable
   :unique-find-anywhere-if
   :variablep
   :variables-in
               
ch 14b
package: :reasoning-expressiveness

*primitives*
??
a
add-fact
args
def-attached-fn
each
fact-present-p
ind
index-new-fact
length=1
mapc-reteieve
maybe-add
query-bind
rel
replace-?-vars
retrieve
retrieve-bagof
retrieve-conjunction
retrieve-fact
retrieve-matches
retrieve-setof
run-attached-fn
sub
test-bears
translate-exp
val

ch 14c
package: :reasoning-possible-worlds

ch 15 
package: :canonical
         
*binary-operators*
*unary-operators*
add-integ-const
args->prefix
canon
canon->prefix
canon-simplifier
coef
copy-poly
def-integ
degree
deriv-poly
eval-poly
expanded-synthetic-division
exponent->prefix
inf-aux
inf-iter
intix->prefix
integ-aux
integ-poly
intersperse
k+poly
k*poly
main-var
make-poly
make-rat
normalize-poly
poly
poly+
poly+poly
poly+same
poly*poly
poly*same
poly/poly
poly^n
poly=
poly-
poly-calc-interval
polynomial
polyp
prefix->canon
prefix->infix
proper-poly-p
rat+rat
rat*rat
rat/rat
rat=
rat-denominator
rat-numerator
ratp
var=
var>
weight
|#





                