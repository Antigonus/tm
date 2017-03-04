#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; 
  (define-condition not-implemented (error)
    ((text :initarg :text :reader text)))


;;--------------------------------------------------------------------------------
;; list-L
;;
  ;; the tm:o function may only be used inside of L (or [])
  (define-condition use-of-o (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; mk
;;
  (define-condition bad-init-value (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; tm decl only implementations
;;
  (define-condition step-from-rightmost (error)
    ((text :initarg :text :reader text)))

  ;; used for #'a and #'d on spill when allocation fails
  (define-condition alloc-fail (error)
    ((text :initarg :text :reader text)))

  (define-condition dealloc-on-rightmost (error)
    ((text :initarg :text :reader text)))

  (define-condition dealloc-collision (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; tm with status

  ;; machine was abandoned to the garbage collector, but someon uses it
  (define-condition operation-on-abandoned (error)
    ((text :initarg :text :reader text)))

  (define-condition use-of-empty (error)
    ((text :initarg :text :reader text)))

  ;; the head is parked, but someone tries to read or write through it
  (define-condition access-through-parked-head (error)
    ((text :initarg :text :reader text)))
 

;;--------------------------------------------------------------------------------
;; dataflow
;;
  (define-condition not-ready (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; buffers
;;
  (define-condition dequeue-from-empty (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; worker
;;
  ;; nothing has been allocated at this location
  (define-condition worker-must-have-src-or-dst (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; worker-utilities
;;
  (define-condition binner-no-such-bin (error)
    ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; tm-aggregate
;;
  ;; nothing has been allocated at this location
  (define-condition object-not-tape-machine (error)
    ((text :initarg :text :reader text)))



;;--------------------------------------------------------------------------------
;; access lang
;;
  (define-condition Δ-malformed-access-program (error)
    ((text :initarg :text :reader text)))

  (define-condition Δ-unrecognized-command (error)
    ((text :initarg :text :reader text)))

  (define-condition Δ-required-arg-missing (error)
    ((text :initarg :text :reader text)))
  
