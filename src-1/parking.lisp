#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A parked head acts like a head on a padding cell.  Actions that occur to the 
right of the cell are all valid.  However, actions that would occur on the 
cell are not.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; head parking - moving the head into and out of the address space
;;

  (defun parked (tm)
    "True iff tape machine head is parked."
    (∨
      (typep tm 'tm-void)
      (typep tm 'tm-parked-singular)
      (typep tm 'tm-parked-tape)
      ))

  ;; this is the more common call for synch
  (defun not-parked (tm)
    "True iff tape machine head is not parked."
    (¬ (parked tm))
    )

  (defun is-void (tm)
    "True iff the machine is void."
    (typep tm 'tm-void)
    )
    
  (defun not-void (tm)
    "True iff the tape machine tape is not void."
    (¬ (is-void tm))
    )

  ;; the return value from park should ignored
  (defgeneric park (tm))

  (defmethod park ((tm tape-machine))
    (let(
          (instance (mk 'tm-parked-tape))
          )
      (init instance {:base tm}
        #'do-nothing
        (λ()(error 'impossible-to-get-here))
        )))

