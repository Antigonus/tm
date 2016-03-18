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
      (cont◨ (be ∅))
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

    (s src cont-ok cont◨)
    )
