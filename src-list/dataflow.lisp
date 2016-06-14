#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; synchronizing inputs
;;
  (defun sync
    (
      tms 
      &optional 
      (pred #'is-active)
      (cont-ready (be t))
      (cont-not-ready 
        (λ(retry tms)
          (declare (ignore retry tms))
          ∅
          )))
    (labels(
             (retry ()
               (∀ tms (λ(tms)(funcall pred (r tms)))
                 cont-ready
                 (λ()(funcall cont-not-ready #'retry tms))
                 ))
             )
      (retry)
      ))
