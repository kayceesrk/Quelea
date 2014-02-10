(define-sort Set (T) (Array T Bool))
(declare-sort Action)
(declare-datatypes () ((ActionPair (mk-pair (first Action) (second Action)))))

(define-sort ActionSoup () (Set Action))

(define-sort VisRel () (Set ActionPair))

;; ----------------------------------------------------------------------------

(declare-const emptyAS (ActionSoup))
(assert (= emptyAS ((as const (ActionSoup)) false)))

(declare-const emptyVis (VisRel))
; (assert (= emptyVis ((as const (VisRel)) false)))

;; ----------------------------------------------------------------------------
;; Space for working

(declare-const a (Action))
(declare-const b (Action))
(declare-const c (Action))
(declare-const d (Action))
(assert (distinct a b c d))

(declare-const as (ActionSoup))

(assert (= as (store (store (store (store emptyAS a true) b true) c true) d true)))

(declare-const vis (VisRel))
(assert (= vis (store (store (store emptyVis (mk-pair a b) true) (mk-pair b c) true) (mk-pair c d) true)))

;; ----------------------------------------------------------------------------
;; Helper functions

(define-fun visTo ((x Action) (y Action)) Bool
	(select vis (mk-pair x y))
)

;; ----------------------------------------------------------------------------

;; visibility only relates actions in action soup
(assert (forall ((x Action) (y Action))
								(=> (or (not (select as x)) (not (select as y)))
								(and (not (visTo x y)) (not (visTo y x))))
				)
)

;; visibility is irreflexive
(assert (forall ((x Action)) (not (visTo x x))))


;; visibility is asymmetric
(assert (forall ((x Action) (y Action)) (=> (visTo x y) (not (visTo y x)))))

;; visibility is transitive
(assert (forall ((x Action) (y Action) (z Action))
								(=> (and (visTo x y) (visTo y z)) (visTo x z))
				)
)

(echo "--Before total order assertion--")
(check-sat)



;; visibility is totally ordered
(define-fun totalOrder () Bool
	(forall ((x Action) (y Action))
					(=> (and (select as x) (select as y) (not (= x y)))
							(or (visTo x y) (visTo y x)))
	)

)

(push)
(echo "--After total order assertion--")
(assert (not totalOrder))
(check-sat)
(pop)
