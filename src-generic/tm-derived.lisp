#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are derived just from the primitives, so there is no functional need for a
new tape machine implementation to specialize them.


|#
(in-package #:tm0)

;;--------------------------------------------------------------------------------
;; cueing
;;  
  (defgeneric cue-rightmost (tm)
    (:documentation
      "Cue tm's head to the rightmost cell."
      ))

  ;; step does not move forward from rightmost, rather takes the rightmost continuation
  (defmethod cue-rightmost ((tm tape-machine))
    (declare (ignore state cont-void))
    (labels(
             (work() (s tm #'work #'do-nothing))
             )
      (work)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defun as
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Like #'a, but tm is stepped to the new cell"
    (a tm object 
      (λ()(s tm cont-ok #'cant-happen))
      cont-no-alloc
      ))

  ;; append with contract that head is at rightmost
  (defun a&h◨ 
    (
      tm 
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'a with a contract that the head is on rightmost."
    (a&h◨-0 tm object cont-ok cont-no-alloc)
    )
  (defgeneric a&h◨-0 (tm object cont-ok cont-no-alloc))
  ;; some specializations can make better use of this contract
  (defmethod a&h◨-0 ((tm tape-machine) object cont-ok cont-no-alloc)
    (a tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )

  ;; append, and head is at rightmost, then step
  (defun a&h◨s
    (
      tm 
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "#'as with a contract that the head is on rightmost."
    (a&h◨-0 tm object cont-ok cont-no-alloc)
    )
  (defgeneric a&h◨s-0 (tm object cont-ok cont-no-alloc))
  ;; some specializations can make better use of this contract
  (defmethod a&h◨s-0 ((tm tape-machine) object cont-ok cont-no-alloc)
    (as tm object (λ()(s tm)(funcall cont-ok)) cont-no-alloc)
    )



