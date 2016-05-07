#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

In the English language, 'dequeue' means to remove something from a queue.

A tm-stack, or a tm-queue, are implemented in regions of space:

          rightmost of region
          bottom of the stack
          enqueue values for the queue with #'a◨s
          |
          |
     L .. R
     |
     |
     leftmost of region
     top of the stack
     dequeue values for the queue or stack with d◧
     enqueue values for the stack with a◧

Note that cue-rightmost is an efficient operation on regions. By
extention the generic a◨ is efficient.  This is why we use regions for
the buffer objects.

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
    "Enqueues object.  Note effiency issues as it refers to rightmost."
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
  (defclass buffer()
    ((region
       :initform
       (let*(
              (base (mk 'tm-list))
              )
         (as base 'buffer) ; assures that the region location will not be void
         (mk 'tm-region :base base)
         )
      :initarg :region
      :accessor region
      )))

  (defgeneric enqueue (buffer object))
  (defgeneric dequeue (buffer &optional cont-ok cont-empty))
  (defgeneric empty (buffer &optional cont-true cont-false))

  (defmethod empty 
    (
      (buffer buffer)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if 
      (typep (region buffer) 'tm-void)
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; stack
;;
  (defclass stack (buffer)())

  (defmethod dequeue 
    (
      (stack stack)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue (region stack) cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (stack stack)
      object
      )
    (stack-enqueue (region stack) object)
    )
    
;;--------------------------------------------------------------------------------
;; queue
;;
;; queue is represented by tm-h◨,  and tm-h◧ is recovered using (cue-leftmost)
;;
  (defclass queue (buffer)())

  (defmethod dequeue 
    (
      (queue queue)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (queue-dequeue (region queue) cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (queue queue)
      object
      )
    (queue-enqueue (region queue) object)
    )
