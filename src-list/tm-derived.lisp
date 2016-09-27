#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are derived just from the primitives, so there is no functional need for a
new tape machine implementation to specialize them.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cueing
;;  
  (defgeneric cue-rightmost (tm)
    (:documentation
      "Cue tm's head to the rightmost cell."
      ))

  ;; step does not move forward from rightmost, rather takes the rightmost continuation
  (defmethod cue-rightmost ((tm tape-machine))
    (labels(
             (work() (s tm #'work (be t)))
             )
      (work)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defgeneric as
    (
      tm
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Like #'a, but tm is stepped to the new cell
      "))

  (defmethod as
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (a tm object 
      (λ()(s tm cont-ok #'cant-happen))
      cont-no-alloc
      ))

  (defgeneric a&h◨ 
    (
      tm
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "#'a with a contract that the head is on rightmost.
      "))

  (defmethod a&h◨ 
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    ;; specializations might make better use of the contract
    (a tm object cont-ok cont-no-alloc)
    )

  (defgeneric as&h◨ 
    (
      tm
      object
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "#'as with a contract that the head is on rightmost.
      "))

  (defmethod as&h◨ 
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    ;; specializations might make better use of the contract
    (as tm object cont-ok cont-no-alloc)
    )

