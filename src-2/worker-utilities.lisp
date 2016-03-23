#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; bins objects
;;
;; source from one tm
;; destination to many tms
;; accepts an op
;;    result from op is taken as an integer index into dsts
;;
  (def-worker 
    binner
    src 
    dsts ; a tm with members that are dst tms
    op
    (&optional 
      (cont-ok (be t)) 
      (cont-rightmost (be ∅))
      (cont-no-such-bin (λ(bin)(declare(ignore bin))(error 'binner-no-such-bin)))
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
      )

    (let*(
           (object (r src))
           (bin (funcall op object))
           )
      (if
        (< bin 0) 

        (funcall cont-no-such-bin bin)

        (r-index dsts bin
          (λ(tm-dst)
            ;;(print {"writing" object "tm-dst bin:" bin})
            (as tm-dst object #'do-nothing cont-no-alloc)
            )
          (λ()(funcall cont-no-such-bin bin))
          )))

    (s src cont-ok cont-rightmost)
    )

  (defun test-binner-0 ()
    (let(
          (tm-src (tm-mk 'tm-list [2 21 12 42 43 47]))
          (tm-dsts (tm-mk 'tm-list {(tm-mk 'tm-list) (tm-mk 'tm-list)}))
          )
      (labels(
               (op (object) (if (evenp object) 0 1))
               (worker (cont-loop cont-return) 
                 (binner tm-src tm-dsts #'op {cont-loop cont-return})
                 )
               )
        (⟳ #'worker)
        )

      (let(
            (bin-0 (cdr (to-list (r-index tm-dsts 0))))
            (bin-1 (cdr (to-list (r-index tm-dsts 1))))
            )
        ;;(print {"bin-0" bin-0})
        ;;(print {"bin-1" bin-1})
        (∧
          (equal bin-0 [2 12 42])
          (equal bin-1 [21 43 47])
          ))))
  (test-hook test-binner-0)

  (defun test-binner-1 ()
    (let(
          (tm-src (tm-mk 'tm-list [2 21 12 42 43 47]))
          (tm-dsts (tm-mk 'tm-list {(tm-mk 'tm-list) (tm-mk 'tm-list)}))
          )

      (⟳ (λ(cont-loop cont-return)
           (binner tm-src tm-dsts (λ(i)(if (evenp i) 0 1)) {cont-loop cont-return})
           ))

      (let(
            (bin-0 (cdr (to-list (r-index tm-dsts 0))))
            (bin-1 (cdr (to-list (r-index tm-dsts 1))))
            )
        ;;(print {"bin-0" bin-0})
        ;;(print {"bin-1" bin-1})
        (∧
          (equal bin-0 [2 12 42])
          (equal bin-1 [21 43 47])
          ))))

  (test-hook test-binner-1)
