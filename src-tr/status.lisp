#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Wraps another tape machine so as to provide and use status information 
about the lower machine.  Status is either 'empty, 'parked, 'active.

Base machines must have an initializer that accepts an :init keyword. Note
that status machines currently do not have an :init initializer keyword.

The right neighbor of a parked head, is the leftmost cell of the base machine's tape.  We
own the base machine, so it can not simultaneously be used by other code.

If we include 'abandoned as a status, then the entire interface will
have to be reproduced here, and inheriting from identity does nothing.
But the same can be said of 'empty. So we may as well keep abandoned.
.. well at least we get to inherit base slot from identity..


|#

(in-package #:tm)



;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (def-type status-tr (identity-tr)
    (
      (status
        :initarg status
        :accessor status
        )
      (type
        :initarg type
        :accessor type
        )))


;;--------------------------------------------------------------------------------
;; new functions
;;
  (defun is-legal-status (status &optional (ct (be t)) (c∅ (be ∅)))
    (case status
      (('abandoned 'empty 'parked 'active) [ct])
      (otherwise [c∅])
      ))

  ;;; scoped make

  ;;; abandon, park


;;--------------------------------------------------------------------------------
;; making status transforms
;;
  (defun-typed init 
    (
      (tm status-tr)
      (init-value cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (destructuring-bind
        (&key base type) init-value
        (cond
          (base
            (setf (base tm) base)
            (setf (type tm) (type-of base))
            (setf (status tm) 'active)
            [➜ok tm]
            )
          ((∧ base type) [➜fail])
          (type
            (setf (type tm) type)
            (setf (status tm) 'empty)
            [➜ok tm]
            )
          (t [➜fail])
          ))))

  (defun-typed init 
    (
      (tm status-tr)
      (init-value status-tr)
      &optional ➜
      )
    (destructing-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (case (status init-value)
        ('abandoned
          [➜fail]
          )
        (('active 'parked)
          (setf (base tm) (mk (type-of (base tm)) tm)) ; makes an entangled copy
          (setf (type tm) (type init-value))
          (setf (status tm) (status init-value))
          [➜ok tm]
          )
        ('empty
          (setf (type tm) (type init-value))
          (setf (status tm) 'empty)
          [➜ok tm]
          )
        )))


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defmacro def-status-tr-1 (f &rest args)
    `(if
       (eq (status tm) 'active)
       (,f (base tm) ,@args ➜)
       (destructuring-bind
         (
           &key
           (➜empty     #'use-of-empty)
           (➜parked    #'parked-head-use)
           &allow-other-keys
           )
         ➜
         (case (status tm)
           ('abandoned (operation-on-abandoned))
           ('empty     [➜empty])
           ('parked    [➜parked])
           (otherwise  [cant-happen])
           ))))

  (def-status-tr-1 r)

  (defun-typed esr ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (esr (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜rightmost (be ∅))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty     [➜rightmost])
          ('parked    (r (base tm) ➜))
          (otherwise  [cant-happen])
          ))))

  (def-status-tr-1 w instance)

  (defun-typed esw ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (esw (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜rightmost (be ∅))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty     [➜rightmost])
          ('parked    (w (base tm) ➜))
          (otherwise  [cant-happen])
          ))))

  (defun-typed cue-leftmost ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (cue-leftmost (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜empty     #'use-of-empty)
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty     [➜empty])
          ('parked
            ;; base machine will already have its head on leftmost, all parked machines do
            (setf (status tm) 'active)
            )
          (otherwise  [cant-happen])
          ))))

  (defun-typed s ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (s (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜ok        (be t))
          (➜rightmost (be ∅))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty     [➜rightmost])
          ('parked
            (setf (status tm) 'active)
            [➜ok]
            )
          (otherwise  [cant-happen])
          ))))

  (defun-typed a ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (a (base tm) instance ➜)
      (destructuring-bind
        (
          &key
          (➜ok        (be t))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty
            (setf (base tm) (mk (type tm) {:tape {instance}}))
            (setf (status tm) 'parked)
            [➜ok]
            )
          ('parked
            (a◧ (base tm) instance)
            [➜ok]
            )
          (otherwise  [cant-happen])
          ))))

  (defun-typed on-leftmost ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (on-leftmost (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜∅        (be ∅))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          (('empty 'parked) [➜∅])
          (otherwise  [cant-happen])
          ))))

  (defun-typed on-rightmost ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (on-rightmost (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜∅        (be ∅))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          (('empty 'parked) [➜∅])
          (otherwise  [cant-happen])
          ))))
        
;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (defun-typed cue-rightmost ((tm status-tr) &optional ➜)
    (if
      (eq (status tm) 'active)
      (cue-rightmost (base tm) ➜)
      (destructuring-bind
        (
          &key
          (➜empty     #'use-of-empty)
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty     [➜empty])
          ('parked
            (setf (status tm) 'active)
            (cue-rightmost (base tm) ➜)
            )
          (otherwise  [cant-happen])
          ))))
  
  (defun-typed as ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (as (base tm) instance ➜)
      (destructuring-bind
        (
          &key
          (➜ok        (be t))
          (➜rightmost (be t))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty
            (setf (base tm) (mk (type tm) {:tape {instance}}))
            (setf (status tm) 'active)
            [➜ok]
            )
          ('parked
            (w (base tm) instance)
            (setf (status tm) 'active)
            [➜ok]
            )
          (otherwise  [cant-happen])
          ))))

  ;; we specify these so that we won't lose the contract
  (defun-typed a&h◨ ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (a&h◨ (base tm) instance ➜)
      (a instance ➜)
      ))

  (defun-typed as&h◨ ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (as&h◨ (base tm) instance ➜)
      (as instance ➜)
      ))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-tr) instance &optional ➜)
    (if
      (case (status tm)
        (('active 'parked)
          (a◧ (base tm) instance ➜)
          )
        (otherwise
          (destructuring-bind
            (
              &key
              (➜ok        (be t))
              &allow-other-keys
              )
            ➜
            (case (status tm)
              ('abandoned (operation-on-abandoned))
              ('empty
                (setf (base tm) (mk (type tm) {:tape {instance}}))
                (setf (status tm) 'parked)
                [➜ok]
                )
              )
              (otherwise  [cant-happen])
              )))))
        
        
  (defun-typed d ((tm status-tr) &optional spill ➜)
    (if
      (eq (status tm) 'active)
      (d (base tm) spill ➜) ; due to collisions d can not make the machine empty
      (destructuring-bind
        (
          &key
          (➜ok        (be t))
          (➜rightmost (be t))
          &allow-other-keys
          )
        ➜
        (case (status tm)
          ('abandoned (operation-on-abandoned))
          ('empty [➜rightmost])
          ('parked (d◧ tm instance ➜)
          (otherwise  [cant-happen])
          ))))

    ;; status machine adds an additional continuation, that of cont-rigthmost
    ;; this can not happen on machines which always have a head on the tape
    (defun-typed d◧ ((tm status-tr) &optional spill ➜)
      (if
        (eq (status tm) 'active)
        (d◧ tm spill ➜)
        (destructuring-bind
          (&key
            (➜ok        (be t))
            (➜rightmost (be t))
            (➜no-alloc #'alloc-fail)
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('abandoned (operation-on-abandoned))
            ('empty [➜rightmost])
            ('parked
              (on-rightmost (base tm) ; head is on leftmost, also on rightmost?
                { :➜t (λ() ; if so, then we are deleting the last cell
                        (setf (base tm) ∅)
                        (setf (status tm) 'empty)
                        )
                  :➜∅ (λ()
                        (s (base tm)
                          {:➜ok #'do-nothing :➜rightmost #'cant-happen}
                          )
                        (d◧ (base tm)
                          {:➜ok #'do-nothing :➜no-alloc ➜no-alloc :➜collision #'cant-happen}
                          ))
                  }))
            (otherwise  [cant-happen])
            ))))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
    
