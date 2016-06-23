#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

In the English language, 'dequeue' means to remove something from a queue.

A tm-stack, or a tm-queue, are implemented in regions of space:

          rightmost of region
          bottom of the stack
          enqueue values for the queue with #'a◨
          |
          |
     L .. R
     |
     |
     leftmost of region
     top of the stack
     dequeue values for the queue or stack with d◧
     enqueue values for the stack with a◧

Use a tm-region to make a◨ efficient, and thus enqueue of values onto a queue object
efficient.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  tape machine used as a stack
;;
  (defun stack-enqueue (tm object)
    "Pushes an object on to the stack"
    (a◧ tm object)
    )

  ;; it is pronounced d-q
  (defun stack-dequeue
    (
      tm
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty :text "stack is empty")))
      )
    "Pulls an object off the top of the stack"
    (d◧ tm ∅ cont-ok cont-empty)
    )

;;--------------------------------------------------------------------------------
;;  tape machine used as a queue
;;
  (defun queue-enqueue (tm object)
    "Enqueues object.  Note effiency issues as it refers to rightmost."
    (a◨ tm object)
    )

  (defun queue-dequeue
    (
      tm
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    "Dequeus object"
    (d◧ tm ∅ cont-ok cont-empty)
    )

;;--------------------------------------------------------------------------------
;;  queues and stacks as types sharing this interface:
;;
  (defgeneric enqueue (buffer object))
  (defgeneric dequeue (buffer &optional cont-ok cont-empty))
  (defgeneric buffer-empty (buffer &optional cont-true cont-false))
;; (defgeneric print-buffer ..)

;;--------------------------------------------------------------------------------
;; stack
;;
;; stack virtualization layer facilitates the specialization of the
;; enqueue/dequeue interface.  Another specialization is that of queue
;;
  (defclass stack ()
    (
      (buffer 
        :initarg :buffer 
        :accessor buffer
        )
      ))

  ;; stack is not a tm but this works .. see length.lisp
  (defmethod buffer-empty ((stack stack) &optional (cont-true (be t)) (cont-false (be ∅)))
    (empty (buffer stack) cont-true cont-false)
    )

  (defmethod dequeue 
    (
      (stack stack)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue (buffer stack) cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (stack stack)
      object
      )
    (stack-enqueue (buffer stack) object)
    )
    
;;--------------------------------------------------------------------------------
;; queue
;;
;; queue is represented by tm-h◨,  and tm-h◧ is recovered using (cue-leftmost)
;;
  (defclass queue ()
    (
      (buffer 
        :initarg :buffer 
        :accessor buffer
        )
      ))

  ;; stack is not a tm but this works .. see length.lisp
  (defmethod buffer-empty ((queue queue) &optional (cont-true (be t)) (cont-false (be ∅)))
    (empty (buffer queue) cont-true cont-false)
    )

  (defmethod dequeue 
    (
      (queue queue)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (queue-dequeue (buffer queue) cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (queue queue)
      object
      )
    (queue-enqueue (buffer queue) object)
    )
