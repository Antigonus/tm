#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We never make abandoned machines, rather managed machines are change classed
to this type after they are deallocated.

An abandoned machine is one that the programmer requested of the manager
to deallocate.  Once a managed tm is deallocated, it should not be used. Hence
here all methods throw an error 'operation-on-abandoned'.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type abandoned-mtm (managed-tm))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmacro def-abandoned-1 (f &rest args)
    `(defun-typed f ((tm abandoned-mtm) ,@args &optional ➜)
       (declare (ignore ,@args))
       (operation-on-abandoned)
       )
    )

  (def-abandoned-1 r)
  (def-abandoned-1 esr)
  (def-abandoned-1 w instance)
  (def-abandoned-1 esw instance)

  (def-abandoned-1 r◧)
  (def-abandoned-1 w◧ instance)

  (def-abandoned-1 cue-leftmost)
  (def-abandoned-1 s)
  (def-abandoned-1 a instance)
  (def-abandoned-1 on-leftmost)
  (def-abandoned-1 on-rightmost)

;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (def-abandoned-1 cue-rightmost)
  (def-abandoned-1 as instance) 
  (def-abandoned-1 a&h◨ instance)
  (def-abandoned-1 as&h◨ instance)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (def-abandoned-1 a◧ instance)
  (defun-typed d
    (
      (tm abandoned-mtm)
      &optional spill ➜
      )
    (declare (ignore tm))
    (operation-on-abandoned)
    )
  (defun-typed d◧
    (
      (tm abandoned-mtm)
      &optional spill ➜
      )
    (declare (ignore tm))
    (operation-on-abandoned)
    )


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell
    (
      (tm0 abandoned-mtm)
      (tm1 abandoned-mtm)
      &optional ➜
      )
    (declare (ignore tm0 tm1))
    (operation-on-abandoned)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 abandoned-mtm)
      (tm1 tape-machine)
      &optional ➜
      )
    (declare (ignore tm0 tm1))
    (operation-on-abandoned)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 abandoned-mtm)
      &optional ➜
      )
    (declare (ignore tm0 tm1))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;
  (defun-typed s≠ 
    (
      (tm0 abandoned-mtm)
      (tm1 abandoned-mtm)
      &optional ➜
      )
    (declare (ignore tm0 tm1))
    (operation-on-abandoned)
    )

  (def-abandoned-1 a◨ instance)

;;--------------------------------------------------------------------------------
;; entanglement
;;
  (defun-typed with-mk-entangled
    (
      (tm0 abandoned-mtm)
      continuation
      )
    (declare (ignore tm0 continuation))
    (operation-on-abandoned)
    )
