#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A new tape machine may be made by calling tm-mk.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; the base type
;;
;;  HA holds the head state.  cue-leftmost resets this HA without refering
;;  to its prior value.  Park uses HA for a different purpose, that of remembering
;;  the type of machine that was parked.
;;
;;  tape is the stuff that has been added to the container.  This is set to ∅
;;  when the machine goes to tm-void. The tape is restored with the function a◧.
;;
;;  parameters holds characterizing information unique to the instance.  For example for
;;  our integer recurrance generator, it is a struct that holds the min value (start
;;  value), the max value, and the increment.
;;
;;  entanglments is a list of machines that share the tape.
;;
  (defclass tape-machine ()
    (
      (HA ; locates a cell on the tape
        :initarg :HA 
        :accessor HA
        )
      (tape ; a sequence of cells, each that may hold an object
        :initarg :tape
        :accessor tape
        )
      (parameters ; for such things as generator that need seed values
        :initarg :parameters
        :accessor parameters
        )
      (entanglements ; list of other tape-machines that share the same tape
        :initarg entanglements
        :accessor entanglements
        )
      ))


   (defun indent (n) 
     (dotimes (i n)(princ "  "))
     )

   (defun print-tape-machine-1 (tm &optional (n 0))
     (indent n) (princ tm) (nl)
     (indent n) (princ "HA: ") (princ (HA tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     (indent n) (princ "parameters: ") (princ (parameters tm)) (nl)
     )

   (defun print-tape-machine (tm &optional (n 0))
     (indent n) (princ tm) (nl)
     (indent n) (princ "HA: ") (princ (HA tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     (indent n) (princ "parameters: ") (princ (parameters tm)) (nl)
     (indent n) (princ "entanglements:") (nl)
     (let(
           (es (entanglements tm))
           )
       (when es
         (cue-leftmost es))
         (⟳(λ(cont-loop cont-return)
             (if 
               (eq (r es) tm)
               (progn (indent (1+ n)) (princ "#self")(nl))
               (print-tape-machine-1 (r es) (1+ n))
               )
             (s es cont-loop cont-return)
             ))))


;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified type to hold the specified objects
;;
;;  init-list is a keyword list.  
;;
;;  :tm-type is used by empty to know the tape of tape to create should cells be
;;    added to the empty.
;;
;;  :mount is used to specify initial objects for a tape machine.  
;;
;;  :seed is used to give seed parameters to generators.
;;
;;  :base provides the base machine for a transforms.
;;
;;  :rightmost marks the rightmost cell for regions
;;
;;  Other keywords should not override these, as they are used in the init
;;  routines to detect valid initialization expressions.
;;
  (defgeneric init (instance init-list &optional cont-ok cont-fail))

  (defmethod init 
    (
      instance 
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ() (error 'unrecognized-instance-type)))
      )
    (declare (ignore instance init-list cont-ok))
    (funcall cont-fail)
    )

  (defun mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (init instance init-list)
      instance
      ))

;;--------------------------------------------------------------------------------
;;  given a sequence return a tape machine over that sequence
;;    sequences are things we can step into, and that tree traversal will
;;    consider to be something to traverse.
;;
;;  note that tm-derived provides a generic mount for mounting a 
;;  tape machine to a tape machine, which is similar to dup, except the
;;  head goes to leftmost
;;
  (defgeneric mount (sequence &optional cont-ok cont-fail))

  (defmethod mount
    (
      sequence 
      &optional 
      (cont-ok #'echo)
      (cont-fail (λ()(error 'mount-unrecognized-sequence-type)))
      )
    (declare (ignore sequence cont-ok))
    (funcall cont-fail)
    )

