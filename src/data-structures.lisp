#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  stack as operators on a tape
;;
;;  When the stack extends to the end of the tape it can be represented with
;;  just tm@.  The empty test is not really needed, as dequeue has an empty stack
;;  continuation.  However, (on-rightmost tm@) is an empty test.
;;
;;  For an embedded stack, once a valid tm◨ is found, it remains valid, as new
;;  objects are given new cells.  So it can be found by stepping from leftmost
;;  after the first push.
;;
  (defun stack-enqueue (tm@ object)
    "tm@ is on an attachment point for a stack.
     Everything to the right of tm@ is considered to be on the stack.
    "
    (a. tm@ object)
    )

  ;; in the English language, dequeue means to remove something from a queue
  ;; it is pronounced d-q
  (defun stack-dequeue
    (
      tm@ 
      &optional 
      (cont-ok #'echo) 
      (cont-empty (error 'dequeue-from-empty :text "stack is empty"))
      )
    (d tm@ 'r
      (λ(object)(funcall cont-ok object))
      cont-empty
      ))

  ;; a stack can be embedded, provided that we record tm◨ of for the 
  ;; stack after the first push.
  (defun embedded-stack-dequeue
    (
      tm@ 
      tm◨
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty :text "stack is empty")))
      )
    (tms-on-same-cell tm@ tm◨ 
      cont-empty
      (λ() 
        (d tm@ 'r
          (λ(object)(funcall cont-ok object))
          cont-empty
          )))
    )

  ;; or non-embedded-empty, use on-rightmost
  ;; this works for stacks or queues
  (defun embedded-empty (tm@ tm◨ &optional (cont-true (be t)) (cont-false (be ∅)))
    "To get tm◨ for stack, dup tm@ after the first en-stack operation, then step."
    (tms-on-same-cell tm@ tm◨ cont-true cont-false)
    )

;;--------------------------------------------------------------------------------
;;  queue as operators on a tape
;;
;;  When the whole tape is a queue it is possible to use tm◨ to represent the queue, and
;;  then to recover tm@ with (cue-to-leftmost (dup tm◨)).
;;
  (defun queue-enqueue (tm◨ object)
    "tm◨ is the rightmost cell of the queue, but not necessarily of the tape."
    (a tm◨ object)
    )

  (defun queue-dequeue
    (
      tm@ 
      &optional 
      (cont-ok #'echo) 
      (cont-empty (λ()(error 'dequeue-from-empty :text "stack is empty")))
      )
    (if
      (on+1 tm@)
      (progn
        (cue-leftmost tm@)
        (d tm@ 'r cont-ok cont-empty)
        )
      (let(
            (tm1 (dup tm@))
            )
        (cue-leftmost tm1)
        (stack-dequeue tm1 cont-ok cont-empty)
        )))

  (defun embedded-queue-dequeue
    (
      tm@ 
      tm◨
      &optional 
      (cont-ok #'echo) 
      (cont-empty (error 'dequeue-from-empty :text "stack is empty"))
      )
    (embedded-stack-dequeue tm@ tm◨ cont-ok cont-empty)
    )


;;--------------------------------------------------------------------------------
;;  queues and stacks as objects of their own
;;
  ;; the entire tape machine is used for the buffer (i.e. the buffer is not embedded)
  ;; the leftmost cell on the tape is the attachment point, i.e. a padding cell
  (defclass buffer(tape-machine)())

  (defgeneric enqueue (buffer object)) ; push is already reserved as a function name
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
      (buf stack)
      &optional 
      (cont-ok #'echo)
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (stack-dequeue buf cont-ok cont-empty)
    )

  (defmethod enqueue
    (
      (buf stack)
      object
      )
    (stack-enqueue buf object)
    )
    
;;--------------------------------------------------------------------------------
;; embedded stack
;;


;;--------------------------------------------------------------------------------
;; queue
;;
  (defclass queue (buffer)())

  (defmethod dequeue 
    (
      (buf queue)
      &optional 
      (cont-ok #'echo) ; dequeueed object is sent here
      (cont-empty (λ()(error 'dequeue-from-empty)))
      )
    (queue-dequeue buf cont-ok cont-empty)
    )
    
  (defmethod enqueue
    (
      (buf queue)
      object
      )
    (queue-enqueue buf object)
    )

;;--------------------------------------------------------------------------------
;; embedded stack / queue
;;  this would be useful .. should implement it ..
;; ...
