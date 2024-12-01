;;; paip/package.lisp
                    
(defpackage #:02-simple-program
  (:use :cl)
  (:import-from :alexandria
     :mappend))

(defpackage #:lisp-overview
  (:use :cl)
  (:export :proper-list-p))

(defpackage #:general-problem-solver
  (:use :cl)
  (:import-from :alexandria
     :iota
     :mappend
     :shuffle)
  (:export
   :*dbg-ids*
   :dbg
   :dbg-indent
   :debug*
   :find-all
   :starts-with
   :undebug*))

(defpackage #:eliza
  (:use :cl)
  (:import-from :general-problem-solver
     :starts-with)
  (:import-from :alexandria
     :ensure-list
     :mappend
     :random-elt)
  (:export
   :lookup
   :binding-val))

(defpackage #:tools
  (:use :cl)
  (:import-from :alexandria
   :ensure-list
   :xor)
  (:import-from :general-problem-solver
     :*dbg-ids*
     :dbg
     :debug*
     :undebug*)
  (:export
    :?*
    :?+
    :??
    :?and
    :?if
    :?is
    :?not
    :?or
    :?[
    :?]
    :+fail+
    :+no-bindings+
    :a*-search
    :beam-search
    :best-first-search
    :binding-var
    :binding-val
    :breadth-first-search
    :depth-first-search
    :extend-bindings
    :diff
    :expand-pat-match-abbrev
    :get-binding
    :graph-search
    :interactive-interpreter
    :is
    :iter-wide-search
    :lookup
    :make-binding
    :match-variable
    :map-path
    :pat-match
    :pat-match-abbrev
    :path
    :path-saver
    :path-states
    :price-is-right
    :prompt-generator                  
    :rm-tmp
    :rule
    :rule-pattern
    :rule-response
    :rule-based-translator
    :search-all
    :sorter
    :tree-search
    :variablep))

(defpackage #:student
  (:use :cl)
  (:import-from :tools
     :binding-var
     :binding-val
     :expand-pat-match-abbrev
     :extend-bindings
     :get-binding
     :make-binding
     :match-variable
     :pat-match   
     :pat-match-abbrev
     :rule-based-translator
     :variablep)
  (:export
     :prefix->infix))

(defpackage #:symbolic
  (:use :cl :repl-utilities)
  (:import-from :tools
     :rule-based-translator
     :pat-match-abbrev
     :expand-pat-match-abbrev
     :?*
     :?+
     :?is)
  (:export
   :length=1
   :infix->prefix
   :^
   :simplify
   :int
   :d
   :simplify-exp
   :variablep
   :exp
   :exp-args
   :exp-lhs
   :exp-rhs
   :evaluable
   :exp-op
   :expp
   :set-simp-fn
   :*simplification-rules*
   :simplify-by-fn
   :simp-rule
   :clear-props
   :not-number-p
   :x
   :y
   :e
   :undefined
   :d
   :simp-fn
   :find-anywhere
   :expp))

(defpackage #:efficiency
  (:use :cl)
  (:import-from :symbolic
     :length=1
     :infix->prefix
     :^
     :simplify
     :int
     :d
     :simplify-exp
     :variablep
     :exp-lhs
     :exp-rhs
     :exp-op
     :evaluable
     :*simplification-rules*
     :simplify-by-fn)
  (:import-from :repl-utilities
     :trace-package)
  (:import-from :tools
     :pat-match
     :match-variable
     :rule-based-translator)
  (:export
     :with-profiling
     :memoize
     :delay
     :force))

(defpackage #:efficient-simplify
  (:use :cl)
  (:shadow :simplify :simplify-exp)
  (:import-from :tools
     :match-variable
     :pat-match
     :rule-based-translator)
  (:use :symbolic)
  (:import-from :efficiency
     :memoize
     :with-profiling)
  (:export 
     :rules-for
     :index-rules
     :main-op))

(defpackage #:compile-simplify
  (:use :cl)
  (:import-from :efficiency
     :delay
     :force
     :with-profiling)
  (:import-from :tools
     :pat-match
     :get-binding
     :pat-match-abbrev
     :match-variable)
  (:import-from :general-problem-solver
     :starts-with)
  (:import-from :symbolic
     :simp-rule
     :clear-props
     :not-number-p
     :*simplification-rules*
     :set-simp-fn
     :x
     :y
     :^
     :e
     :undefined
     :d
     :simp-fn
     :simplify-by-fn
     :evaluable
     :infix->prefix)
  (:import-from :efficient-simplify
     :rules-for
     :index-rules
     :main-op)
  (:import-from :eliza
     :lookup)
  (:export
   :concat-symbol
   :new-symbol))
     
(defpackage #:more-efficiency
  (:use :cl)
  (:import-from :compile-simplify
     :concat-symbol)
  (:export 
     :reuse-cons))

(defpackage #:logic
  (:use :cl)
  (:import-from :more-efficiency
     :reuse-cons)
  (:export
     :+fail+
     :+no-bindings+
     :*db-predicates*
     :*occurs-check*
     :<-
     :?-
     :add-clause
     :binding-val
     :clause-body
     :clause-head
     :clear-db
     :clear-predicate
     :continuep
     :ensure-list
     :extend-bindings
     :fact
     :get-binding
     :get-clauses
     :lookup
     :match-variable
     :occurs-check
     :predicate
     :prove
     :prove-all
     :rename-variables
     :replace-?-vars
     :rule
     :show-prolog-solutions
     :show-prolog-vars
     :subst-bindings
     :top-level-prove
     :unifier
     :unify
     :unify-variable
     :unique-find-anywhere
     :variablep
     :variables-in))

(defpackage #:compile-logic
  (:use :cl)
  (:import-from :logic  
     :clause-body
     :clause-head
     :clear-predicate
     :continuep
     :get-clauses
     :predicate
     :variablep
     :variables-in)
  (:import-from :general-problem-solver
     :find-all)
  (:import-from :compile-simplify
     :concat-symbol
     :new-symbol)
  (:import-from :symbolic
     :length=1)
  (:import-from :more-efficiency
     :reuse-cons))

(defpackage #:object
  (:use :cl)
  (:import-from :general-problem-solver
   :dbg
   :debug*
   :undebug*))

(defpackage #:reasoning
  (:use :cl :logic))

(defpackage #:reasoning-completeness
  (:use :cl)
  (:import-from :more-efficiency
   :reuse-cons)
  (:export
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
   :copy-dtree
   :dtree
   :dtree-atom-fetch
   :dtree-atoms
   :dtree-fetch
   :dtree-first
   :dtree-index
   :dtree-p
   :dtree-rest
   :dtree-var
   :ensure-list
   :extend-bindings
   :fetch
   :get-binding
   :get-clauses
   :get-dtree
   :index
   :lookup
   :lookup-atom
   :make-dtree
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
   :unifier
   :unify
   :unify-variable
   :unique-find-anywhere-if
   :variablep
   :variables-in))

(defpackage #:reasoning-expressiveness
  (:shadow 
     :retrieve
     :mapc-retrieve
     :retrieve-matches
     :query-bind
     :replace-?-vars)
  (:use :cl :reasoning-completeness)
  (:export
    :*primitives*
    :??
    :a
    :add-fact
    :args
    :def-attached-fn
    :each
    :fact-present-p
    :ind
    :index-new-fact
    :length=1
    :mapc-retrieve
    :maybe-add
    :query-bind
    :rel
    :replace-?-vars
    :retrieve
    :retrieve-bagof
    :retrieve-conjunction
    :retrieve-fact
    :retrieve-matches
    :retrieve-setof
    :run-attached-fn
    :sub
    :test-bears
    :translate-exp
    :val))

(defpackage #:reasoning-possible-worlds
  (:shadow 
     :*primitives*
     :add-fact
     :dtree-index
     :index
     :index-new-fact
     :sub
     :test-bears)
  (:shadowing-import-from 
       :reasoning-expressiveness
    :mapc-retrieve
    :query-bind
    :replace-?-vars
    :retrieve
    :retrieve-matches)
  (:use 
    :cl 
    :reasoning-completeness
    :reasoning-expressiveness))

(defpackage #:canonical
  (:use :cl)
  (:import-from :repl-utilities
   :trace-package)
  (:import-from :symbolic
     :exp-args
     :exp-op
     :expp
     :length=1)
  (:import-from :alexandria
     :mappend)
  (:import-from :general-problem-solver
     :starts-with))

(defpackage #:expert
  (:use :cl))
