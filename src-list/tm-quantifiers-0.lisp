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
  (defun ⟳ (work)
    "⟳ (pronounced \"do\") accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts two
     continuations.  Typically these are called 'cont-loop', and 'cont-return'.  When the
     work function continues with cont-loop, it is immediately called again.  When it
     continues with cont-return, ⟳ returns.
     "
    (labels(
             (do-work ()
               (funcall work
                 #'do-work
                 (λ() (return-from ⟳ t))
                 ))
             )
      (do-work)
      ))

  ;; This version of ⟳ facilitates the programmer in making the stepping function explicit
  ;; as an argument. The machine to be stepped, and the function to use to step it, are
  ;; explicitly provided as arguments.
  ;; 
  (defun ⟳-work-step
    (
      tm 
      &optional 
      (work #'do-nothing) 
      (step #'s)
      )
    "⟳-work-step (pronounced \"do work step\") accepts a tape machine, a function to do
     work, and a step function.  The step function must accept as arguments the tape
     machine, and two continuations.  Typically the continuations are called 'cont-loop' and
     'cont-return'.  For example #'s can be used for stepping.  First the
     work function is called, then the step function is called. If the step function
     continues with cont-loop, ⟳-step repeats.  If it continues with continue-return
     ⟳-step returns.
     "
      (labels(
               (do-work ()
                 (funcall work)
                 (funcall step tm #'do-work (λ()(return-from ⟳-work-step)))
                 )
               )
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
;; careful - the quantifiers start where the head is located, they do not cue-leftmost first
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
    (⟳ (λ(cont-loop cont-return)
         (when 
           (funcall pred tm) 
           (return-from ∃ (funcall cont-true))
           )
         (s tm cont-loop cont-return)
         ))
    (funcall cont-false)
    )

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
        (⟳ (λ(cont-loop cont-return)(∃*-op)(s tm cont-loop cont-return)))
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
;; We avoid this end case by requiring that tms be non-void.
;;
  (defun s-together 
    (
      tms ; tms holds tape machines, tms is not moved, but the constituent machines are
      &optional
      (cont-ok #'echo)
      (cont-exists-on-rightmost (be ∅))
      )
    "s-together is passed a non-void list of tape machines.
    Each time s-together is called, all machines in the list are stepped.
    If any of the machines can not be stepped, then none of the machines are stepped.
    cont-exists-end is called with an iterator on the first of the tms that could not
    be stepped.
    "
    (let(
          (tms0 (fork-0 tms))
          (tms1 (fork-0 tms))
          )
      (if
        (¬∃ tms0 #'on-rightmost)
        (progn
          (⟳ (λ(cont-loop cont-return)
               (s (r tms1) #'do-nothing (λ()(error 'tm-impossible-to-get-here)))
               (s tms1 cont-loop cont-return)
               ))
          (funcall cont-ok 's)
          )
        (funcall cont-exists-on-rightmost)
        )))


;;--------------------------------------------------------------------------------
;; stepping multiple times
;;
  (defgeneric sn (tm n &optional cont-ok cont-rightmost)
    (:documentation 
      "Step n times.  When called, cont-rightmost is passed the current value of n.  For
       example, if the head is on leftmost, and the tape has two cells, and sn is called
       with n set to 3, then the step from rightmost continuation will be called with a
       value of 2.
      "
      ))

  (defmethod sn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(n)(declare (ignore n)) ∅))
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
                 (λ()(return-from sn (funcall cont-rightmost n)))
                 ))
             )
      (count-test)
      (take-step)
      ))

;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (defun csnr 
    (
      tm
      index
      &optional
      (cont-ok #'echo) ; whenever cont-ok is #'echo, other continuations must throw an error
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "fork tm, step n places, then read."
    (csnr-0 tm (state tm) index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric csnr-0 (tm state index cont-ok cont-rightmost cont-parked))
  (defmethod csnr-0 (tm (state void) index cont-ok cont-rightmost cont-parked)
    (declare (ignore tm state cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod csnr-0 (tm (state parked) index cont-ok cont-rightmost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (fork-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(r tm cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod csnr-0 (tm (state active) index cont-ok cont-rightmost cont-parked)
    (let(
          (tm1 (fork-0 tm))
          )
      (sn tm1 index
        (λ()(r tm cont-ok #'cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))

  (defun csnw 
    (
      tm
      object
      index
      &optional
      (cont-ok (be t))
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "fork tm, step n places, then write object."
    (csnw-0 tm (state tm) object index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric csnw-0 (tm state object index cont-ok cont-rightmost cont-parked))
  (defmethod csnw-0 (tm (state void) object index cont-ok cont-rightmost cont-parked)
    (declare (ignore tm state object cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod csnw-0 (tm (state parked) object index cont-ok cont-rightmost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (fork-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(w tm1 object cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod csnw-0 (tm (state active) object index cont-ok cont-rightmost cont-parked)
    (let(
          (tm1 (fork-0 tm))
          )
      (sn tm1 index
        (λ()(w tm1 object cont-ok #'cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))
