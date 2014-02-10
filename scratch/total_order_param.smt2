(define-sort Set (T) (Array T Bool))
(declare-sort Action)
(declare-datatypes () ((ActionPair (mk-pair (first Action) (second Action)))))

(define-sort ActionSoup () (Set Action))

(define-sort VisRel () (Set ActionPair))

;; ----------------------------------------------------------------------------

(declare-const emptyAS (ActionSoup))
(assert (= emptyAS ((as const (ActionSoup)) false)))

(declare-const emptyVis (VisRel))

;; ----------------------------------------------------------------------------
;; Helper functions

(define-fun visTo ((vis VisRel) (x Action) (y Action)) Bool
	(select vis (mk-pair x y))
)

;; ----------------------------------------------------------------------------

;; visibility only relates actions in action soup
(define-fun visRelActSoup ((as ActionSoup) (vis VisRel)) Bool
	(forall ((x Action) (y Action))
								(=> (or (not (select as x)) (not (select as y)))
								(and (not (visTo vis x y)) (not (visTo vis y x))))
	)
)

;; visibility is irreflexive
(define-fun visIrfl ((vis VisRel)) Bool
	(forall ((x Action)) (not (visTo vis x x)))
)

;; visibility is asymmetric
(define-fun visAsym ((vis VisRel)) Bool
	(forall ((x Action) (y Action)) (=> (visTo vis x y) (not (visTo vis y x))))
)

;; visibility is transitive
(define-fun visTrans ((vis VisRel)) Bool
	(forall ((x Action) (y Action) (z Action))
								(=> (and (visTo vis x y) (visTo vis y z)) (visTo vis x z))
	)
)

;; visibility is totally ordered
(define-fun totalOrder ((as ActionSoup) (vis VisRel)) Bool
	(forall ((x Action) (y Action))
					(=> (and (select as x) (select as y) (not (= x y)))
							(or (visTo vis x y) (visTo vis y x)))
	)

)

(echo "Establish basic facts: ")
(check-sat)
(echo "")
(echo "For the rest of the executions, check total order property.")

;; ----------------------------------------------------------------------------
;; Action definitions

(declare-const a (Action))
(declare-const b (Action))
(declare-const c (Action))
(declare-const d (Action))
(assert (distinct a b c d))


;; ----------------------------------------------------------------------------
;; Example 1: a

(push)
(declare-const as (ActionSoup))
(assert (= as (store emptyAS a true)))

(declare-const vis (VisRel))
(assert (= vis emptyVis))

;; establish basic correctness properties
(assert (visRelActSoup as vis))
(assert (visIrfl vis))
(assert (visAsym vis))
(assert (visTrans vis))

;; validate total order property
(echo "Example 1:")
(echo "  Execution: a")
(echo "  Excpect: unsat")
(assert (not (totalOrder as vis)))

(check-sat)
(echo "")
(pop)

;; ----------------------------------------------------------------------------
;; Example 2: a b

(push)
(declare-const as (ActionSoup))
(assert (= as (store (store emptyAS a true) b true)))

(declare-const vis (VisRel))
(assert (= vis emptyVis))


;; establish basic correctness properties
(assert (visRelActSoup as vis))
(assert (visIrfl vis))
(assert (visAsym vis))
(assert (visTrans vis))

;; validate total order property
(echo "Example 2:")
(echo "  Execution: a b")
(echo "  Excpect: sat (a is not visible to b)")
(assert (not (totalOrder as vis)))

(check-sat)
(echo "")
(pop)

;; ----------------------------------------------------------------------------
;; Example 3: a --vis--> b

(push)
(declare-const as (ActionSoup))
(assert (= as (store (store emptyAS a true) b true)))

(declare-const vis (VisRel))
(assert (= vis (store emptyVis (mk-pair a b) true)))


;; establish basic correctness properties
(assert (visRelActSoup as vis))
(assert (visIrfl vis))
(assert (visAsym vis))
(assert (visTrans vis))

;; validate total order property
(echo "Example 3:")
(echo "  Execution: a --vis--> b")
(echo "  Excpect: unsat")
(assert (not (totalOrder as vis)))

(check-sat)
(echo "")
(pop)

;; ----------------------------------------------------------------------------
;; Example 4: a --vis--> b --vis--> c

(push)
(declare-const as (ActionSoup))
(assert (= as (store (store (store emptyAS a true) b true) c true)))

(declare-const vis (VisRel))
(assert (= vis (store (store emptyVis (mk-pair a b) true) (mk-pair b c) true)))


;; establish basic correctness properties
(assert (visRelActSoup as vis))
(assert (visIrfl vis))
(assert (visAsym vis))
(assert (visTrans vis))

;; validate total order property
(echo "Example 4:")
(echo "  Execution: a --vis--> b --vis--> c")
(echo "  Excpect: unsat")
(assert (not (totalOrder as vis)))

(check-sat)
(echo "")
(pop)

;; ----------------------------------------------------------------------------
;; Example 5: a --vis--> b --vis--> c
;;											 |
;;											 \--vis--> d

(push)
(declare-const as (ActionSoup))
(assert (= as (store (store (store (store emptyAS a true) b true) c true) d true)))

(declare-const vis (VisRel))
(assert (= vis (store (store (store emptyVis (mk-pair a b) true) (mk-pair b c) true) (mk-pair b d) true)))


;; establish basic correctness properties
(assert (visRelActSoup as vis))
(assert (visIrfl vis))
(assert (visAsym vis))
(assert (visTrans vis))

;; validate total order property
(echo "Example 5:")
(echo "  Execution: a --vis--> b --vis--> c")
(echo "                        |")
(echo "                        \\--vis--> d")
(echo "  Excpect: sat (execution is a partial order)")
(assert (not (totalOrder as vis)))

(check-sat)
(echo "")
(pop)
