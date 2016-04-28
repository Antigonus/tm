#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

In the English language, 'dequeue' means to remove something from a queue, and 
that is the sense we use the word in this library.

A tm-stack or a tm-queue referred to below is an interval space:

          rightmost of interval
          bottom of the stack
          enqueue values for the queue with #'a◨s
          |
          |
     L .. R
     |
     |
     leftmost of interval
     top of the stack
     dequeue values for the queue or stack with d◧
     enqueue values for the stack with a◧

Use the generic function 'is-empty' to test if a stack or queue is empty.

Note that cue-rightmost is an efficient operation on interval spaces. By
extention the generic a◨ is efficient.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  tape is stack
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
;;  tape is a queue
;;
  (defun queue-enqueue (tm object)
    "Enqueues object"
    (a◨s tm object)
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
;;  queues and stacks as objects of their own
;;
  (defclass buffer(tm-interval)())

  (defgeneric enqueue (buffer object))
  (defgeneric dequeue (buffer &optional cont-ok cont-empty))


;;--------------------------------------------------------------------------------
;; stack
;;
  (defclass stack (buffer)())

  (defmethod dequeue 
    (
      (tm-stack stack)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue tm-stack cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (tm-stack stack)
      object
      )
    (stack-enqueue tm-stack object)
    )
    
;;--------------------------------------------------------------------------------
;; queue
;;
;; queue is represented by tm-h◨,  and tm-h◧ is recovered using (cue-leftmost)
;;
  (defclass queue (buffer)())

  (defmethod dequeue 
    (
      (tm-queue queue)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (queue-dequeue tm-queue cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (tm-queue queue)
      object
      )
    (queue-enqueue tm-queue object)
    )
