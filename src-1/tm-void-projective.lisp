#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The hall of errors, can't do anything with this.

  A singular projective machine, but can't read or write to it.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-void-projective (tm-singular-projective)())

;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-void-projective))
    (declare (ignore tm))
    (error 'tm-void-projective)
    )
  (defmethod w ((tm tm-void-projective) object)
    (declare (ignore tm object))
    (error 'tm-void-projective)
    )
