(declare-sort Action)
(declare-sort Thread)

(declare-fun vis ((Action) (Action)) (Bool))
(declare-fun so ((Action) (Action)) (Bool))
(declare-fun hb ((Action) (Action)) (Bool))
(declare-fun thrd ((Action)) (Thread))

;; visibility is irreflexive
(assert (forall ((x Action)) (not (vis x x))))

;; visibility is anti-symmetric
(assert (forall ((x Action) (y Action))
  (=> (vis x y) (not (vis y x)))
))

;; session order is irreflexive
(assert (forall ((x Action)) (not (so x x))))

;; session order is anti-symmetric
(assert (forall ((x Action) (y Action))
  (=> (so x y) (not (so y x)))
))

;; Session order only relates actions from the same thread.
;; Otherwise, they are unrelated by session order.
(assert (forall ((x Action) (y Action))
  (ite (and (= (thrd x) (thrd y)) (not (= x y)))
       (or (so x y) (so y x))
       (not (or (so x y) (so y x)))
  )
))

;; Session order is transitive
(assert (forall ((x Action) (y Action) (z Action))
  (=> (and (so x y) (so y z))
      (so x z)
  )
))

;; Happens-before follows visibility and session order
(assert (forall ((x Action) (y Action))
  (=> (or (vis x y) (so x y)) (hb x y))
))

;; happens-before is transitive
(assert (forall ((x Action) (y Action) (z Action))
  (=> (and (hb x y) (hb y z)) (hb x z))
))

(echo "Checking the consistency of specifications..")
(check-sat)
(echo "")

(echo "Coordination Free Specifications")
(echo "--------------------------------")
;; read-my-writes
(define-fun rmw_CF () Bool
  (=> (forall ((x Action) (y Action))
         (=> (hb x y) (vis x y))
      )
      (forall ((x Action) (y Action))
         (=> (so x y) (vis x y))
      )
  )
)

(push)
(echo "Read my writes")
(assert (not rmw_CF))
(check-sat)
(pop)

;; Causal visibility
(define-fun cv_CF () Bool
  (=> (forall ((x Action) (y Action))
         (=> (hb x y) (vis x y))
      )
      (forall ((x Action) (y Action) (z Action))
         (=> (and (vis x y) (vis y z)) (vis x z))
      )
  )
)

(push)
(echo "Causal visibility")
(assert (not cv_CF))
(check-sat)
(pop)

(echo "")
(echo "Coordination Required Specifications")
(echo "------------------------------------")

;; total order
(define-fun to_CF () Bool
  (=> (forall ((x Action) (y Action))
         (=> (hb x y) (vis x y))
      )
      (forall ((x Action) (y Action))
         (or (vis x y) (vis y x))
      )
  )
)

(push)
(echo "Total order")
(assert (not to_CF))
(check-sat)
(pop)

;; Weak total order
(define-fun wto_CF () Bool
  (=> (forall ((x Action) (y Action))
         (=> (hb x y) (vis x y))
      )
      (forall ((x Action) (y Action) (z Action))
         (=> (and (vis x z) (vis y z))
             (or (vis x y) (vis y x))
         )
      )
  )
)

(push)
(echo "Weak total order")
(assert (not wto_CF))
(check-sat)
(pop)
