#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Removing the 'embedded' stack/queue, because the same thing can be 
achieved using a subspace.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  tape is stack
;;
;; Leftmost is an attachment point for the stack.
;;
  (defun stack-enqueue (tm-h◧ object)
    "Pulls an object on to the stack"
    (a tm-h◧ object)
    )

  ;; in the English language, dequeue means to remove something from a queue
  ;; it is pronounced d-q
  (defun stack-dequeue
    (
      tm-h◧
      &optional 
      (cont-ok #'echo) 
      (cont-empty (error 'dequeue-from-empty :text "stack is empty"))
      )
    "Pulls an object off of the stack"
    (d tm-h◧ ∅ cont-ok cont-empty)
    )

  (defun stack-empty (tm-h◧ &optional (cont-true (be t)) (cont-false (be ∅)))
    (on-rightmost tm-h◧ cont-true cont-false)
    )

;;--------------------------------------------------------------------------------
;;  queue as operators on a tape
;;
;; tm-h◧ is the pull point, using #'d
;; tm-h◨ is the push point, using #'a
;;
;; for an empty queue tm-h◨ is also leftmost
;;
  (defun queue-enqueue (tm-h◨ object)
    (as tm-h◨ object)
    )

  (defun queue-dequeue
    (
      tm-h◧ 
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue tm-h◧ cont-ok cont-empty)
    )

  (defun queue-empty (tm-h◧ tm-h◨) (heads-on-same-cell tm-h◧ tm-h◨))

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
      (tm-h◧ stack)
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
      (queue-dequeue tm-h◧ cont-ok cont-empty)
      ))
    
  (defmethod enqueue
    (
      (tm-h◨ queue)
      object
      )
    (queue-enqueue tm-h◨ object)
    )

