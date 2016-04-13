#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

In the English language, 'dequeue' means to remove something from a queue, and 
that is the sense we use the word in this library.

A tm-stack or a tm-queue referred to below is an interval space:

          rightmost of interval
          bottom of the stack
          enqueue values for the queue with #'as
          |
          |
     L .. R
     |
     |
     leftmost of interval
     top of the stack
     dequeue values for the queue or stack with d◧
     enqueue values for the stack with a◧

Use the generic function 'is-void' to test if a stack or queue is empty.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  tape is stack
;;
;; Leftmost is an attachment point for the stack.
;;
  (defun stack-enqueue (tm-stack object)
    "Pushes an object on to the stack"
    (a◧ tm-stack object)
    )

  ;; it is pronounced d-q
  (defun stack-dequeue
    (
      tm-stack
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty :text "stack is empty")))
      )
    "Pulls an object off of the stack"
    (d◧ tm-stack ∅ cont-ok cont-empty)
    )


;;--------------------------------------------------------------------------------
;;  queue as operators on a tape
;;
;; tm-attach is the pull point, using #'d
;; tm&h◨ is the push point, using #'a
;;
;; For an empty queue tm-h◨'s and tm-attach's heads are on the same cell.
;;
  (defun queue-enqueue (tm&h◨ object)
    (as tm&h◨ object)
    )

  (defun queue-dequeue
    (
      tm-attach
      tm&h◨
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )

    ;; park the head to prevent having tm-h◨ going into limbo when emptying a queue
    (distance+1
      tm-attach
      tm&h◨ 
      (λ()(cue-to tm&h◨ tm-attach)
      #'do-nothing
      )

    (stack-dequeue tm-attach cont-ok cont-empty)
    )

  (defun queue-empty (tm-attach tm&h◨) (heads-on-same-cell tm-attach tm&h◨))

;;--------------------------------------------------------------------------------
;;  queues and stacks as objects of their own
;;
  ;; the entire tape machine is used for the buffer (i.e. the buffer is not embedded)
  ;; the leftmost cell on the tape is the attachment point, i.e. a padding cell
  (defclass buffer(tape-machine)())

  (defgeneric enqueue (buffer object))
  (defgeneric dequeue (buffer &optional cont-ok cont-empty))

  (defgeneric empty  (buf))

  (defmethod empty ((buf buffer))
    (singleton buf)
    )

;;--------------------------------------------------------------------------------
;; stack
;;
  (defclass stack (buffer)())

  (defmethod dequeue 
    (
      (tm-attach stack)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue tm-h◧ cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (tm-h◧ stack)
      object
      )
    (stack-enqueue tm-h◧ object)
    )
    
;;--------------------------------------------------------------------------------
;; queue
;;
;; queue is represented by tm-h◨,  and tm-h◧ is recovered using (cue-leftmost)
;;
  (defclass queue (buffer)())

  (defmethod dequeue 
    (
      (tm-h◨ queue)
      &optional 
      (cont-ok #'echo) ; dequeueed object is sent here
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (let(
          (tm-h◧ (dup tm-h◨))
          )
      (cue-leftmost tm-h◧)
      (queue-dequeue tm-h◧ tm-h◨ cont-ok cont-empty)
      ))
    
  (defmethod enqueue
    (
      (tm-h◨ queue)
      object
      )
    (queue-enqueue tm-h◨ object)
    )

