#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Generic implementations for making sequence objects

  Some interface routines need to use tm-mk.
  Some tm-mk routines need to use the interface.
  So I have split tm-mk.lisp into two files.
  In this file the generic tape machine interface may be used.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; converts a tape machine to another form
;;

  ;; generic list maker, some specializations, particularly the tm-list specialization,
  ;; will be more efficient.
  (defmethod to-list ((tm0 tape-machine))
    (let(
          (tm1 (tm-mk 'tm-list))
          )
      (⟳ (λ(cont-ok cont◨) (as tm1 (r tm0) cont-ok cont◨)))
      (cdr (tape tm1))
      ))
