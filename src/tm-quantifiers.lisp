#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; looping, see also s-together⟳
;;
  (defun ⟳ 
    (
      tm 
      step 
      &optional 
      (work #'do-nothing) 
      (cont-rightmost (be t))
      )
    "'do'.  Step is a function that accepts a tape machine, a work function, and a
     rightmost continuation. The work function is called, then step is called, and this
     repeats until step takes the rightmost continuation.
     "
    (labels(
             (do-work ()
               (funcall work)
               (funcall step tm #'do-work cont-rightmost)
               ))
      (do-work)
      ))

;;--------------------------------------------------------------------------------
;; quaternion relationship among quantifiers
;;   q00 is existential quantification
;;
  (defmacro q01 (q00 tm pred) 
    `(funcall ,q00 ,tm (λ(i)(not (funcall ,pred i))))
    )

  (defmacro q10 (q00 tm pred) 
    `(not (funcall ,q00 ,tm ,pred))
    )

  (defmacro q11 (q00 tm pred) 
    `(not (funcall ,q00 ,tm (λ(i)(not (funcall ,pred i)))))
    )

;;--------------------------------------------------------------------------------
;; quantification
;;
  (defun ∃ 
    (
      tm
      pred
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "When returning true, tm head is on the first cell that has an object where pred is true.
    When returning false, tm head is on rightmost, and there was no cell where pred was true."
    (⟳ tm #'s
      (λ(the-tm)(when (funcall pred the-tm) (return-from ∃ (funcall cont-true))))
      cont-false
      ))


  ;; same as step-while
  ;; not all objects match pred, here we will show you the first one that doesn't
  ;; there exists an object for which pred is false
  (defun ¬∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, all objects do not match pred, and tm is on the first mismatch."
    (if 
      (q01 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  ;; there does not exist an object for which pred is true
  ;; i.e. pred is false for all objects
  (defun ¬∃ 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, there does not exist an object where pred holds, and tm is at rightmost.
    When false, tm is on an cell with an object where pred holds."
    (if 
      (q10 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  ;; pred is true for all objects
  (defun ∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true all objects match pred.  When false tm will be on the first mismatch."
    (if
      (q11 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∃*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "like ∃, but all objects will be visited"
    (let(
          (result ∅)
          )
      (labels(
               (∃*-op () (when (funcall pred tm) (setq result t)))
               )
        (⟳ tm #'s #'∃*-op)
        (if 
          result
          (funcall cont-true)
          (funcall cont-false)
          ))))

  (defun ¬∀* 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q01 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ¬∃*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q10 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∀*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q11 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))


;;--------------------------------------------------------------------------------
;; stepping multiple machines in unison
;;
;; love the null case.  If there are no tms to step, then there does not
;; exist a tm that stepped, so we should return ∅.  On the other hand, if there
;; are no tms to step, then no tm failed to step, so we should return true. 
;; I avoided this issue by passing in a tm to the sequence of tms. Such a sequence
;; must have one member to exist.
;;
  (defun s-together 
    (
      tms ; tms hold tape machines, tms is not moved, but the constituent machines are
      &optional
      (cont-ok #'echo)
      (cont-exists-on-rightmost (be ∅))
      )
    "s-together is passed a list of tape machines.
    Each time s-together is called, all machines in the list are stepped.
    If any of the machines can not be stepped, then none of the machines are stepped.
    cont-exists-end is called with an iterator on the first of the tms that could not
    be stepped.
    "
    (let(
          (tms0 (dup tms))
          (tms1 (dup tms))
          )
      (if
        (¬∃ tms0 #'on-rightmost)
        (progn
          (⟳ tms1 #'s) ; #'s is the work function
          (funcall cont-ok 'so)
          )
        (funcall cont-exists-on-rightmost)
        )))


;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;

  (defgeneric w* (tm fill &optional cont-rightmost))

  (defmethod w* 
    (
      (tm tape-machine)
      (fill tape-machine)
      &optional 
      (cont-rightmost (be ∅))
      )
    (⟳ fill #'s 
      (λ()
        (w tm (r fill))
        (s tm #'do-nothing (λ()(return-from w* (funcall cont-rightmost))))
        )))


  (defgeneric s* (tm)
    (:documentation 
      "Steps tm to rightmost. It is probably more efficient to cue-to-rightmost."
      ))

  (defmethod s* ((tm tape-machine))
    (⟳ tm #'s)
    )


  (defgeneric a* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Allocates new cells to tm until running out of fill data."
      ))

  (defgeneric as* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Similar to a*, but tm reflects the steps taken."
      ))

  (defun as*-0 
    (
      tm 
      fill
      &optional 
      cont-ok
      cont-no-alloc
      )
    (⟳ fill #'s
      (λ()
        (a tm (r fill) 
          #'do-nothing
          (return-from as*-0 (funcall cont-no-alloc))
          ))
      cont-ok
      ))

  (defmethod a*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (let(
          (tm1 (dup tm0))
          )
      (as*-0 tm1 fill cont-ok cont-no-alloc)
      ))

  (defmethod as*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc (be ∅))
      )
    (as*-0 tm0 cont-ok cont-no-alloc)
    )

  (defgeneric d* (tm &optional spill cont-rightmost cont-no-alloc)
    (:documentation 
      "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right hand side is moved to it.
       If spill is not ∅, and cells can not be moved to it, the objects are
       moved to spill via #'a.  It is only in this last case that we might
       end up taking the cont-no-alloc exit.
      "
      ))

  ;; spill may only be 'drop or a tape machine
  (defmethod d*
    (
      (tm tape-machine)
      &optional 
      spill
      (cont-rightmost (be t))
      (cont-no-alloc (be ∅))
      )
    (labels(
             (do-work()
               (d tm spill #'do-work cont-rightmost cont-no-alloc)
               )
             )
      (do-work)
      ))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric sn (tm n &optional cont-ok cont-rightmost)
    (:documentation 
      "Step count times.  
       When called, cont-rightmost is passed the remaining count."
      ))

  (defmethod sn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (labels(
             (count-test()
               (when (≤ n 0) (return-from sn (funcall cont-ok)))
               )
             (work()
               (decf n)
               (count-test)
               (take-step) ; step from rightmost test is built into #'s
               )
             (take-step()
               (s 
                 tm 
                 #'work
                 (λ()(funcall cont-rightmost n))
                 ))
             )
      (count-test)
      (take-step)
      ))

  (defgeneric an (tm tm-fill count &optional cont-ok cont-rightmost)
    (:documentation 
      "Similar to calling #'a n times on a dup of tm."
      ))

  (defgeneric asn (tm tm-fill n &optional cont-ok cont-rightmost)
    (:documentation 
      "Similar to calling #'as n times. fill is tm that provides initialization
       data. tm and fill are both stepped n times."
      ))

  (defgeneric dn (tm count &optional spill cont-ok cont-rightmost)
    (:documentation
      "Given a tape machine and a natural number.
      Like repeating d count times, but specialized versions might be more efficient.
      "
      ))

  (defmethod dn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (labels(
             (do-work()
               (when (≤ n 0) (return-from dn (funcall cont-ok)))
               (d tm spill 
                 #'do-work
                 (λ()(return-from dn (funcall cont-rightmost n)))
                 )
               (decf n)
               )
             )
      (do-work)
      ))


