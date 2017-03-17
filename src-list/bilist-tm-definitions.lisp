#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (binode-instance (head tm))]
      ))

  (defun-typed esr ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (if
        (binode-right-neighbor (head tm))
        [➜ok (binode-instance (binode-right-neighbor (head tm)))]
        [➜rightmost]
        )))

  (defun-typed w ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (binode-instance (head tm)) instance)
      [➜ok]
      ))

  (defun-typed esw ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (binode-right-neighbor (head tm))
        (progn
          (setf (binode-instance (binode-right-neighbor (head tm))) instance)
          [➜ok]
          )
        [➜rightmost]
        )))

  (defun-typed r◧ ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok (binode-instnace (tape tm))]
      ))

  (defun-typed esr◧ ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (if
        (cdr (tape tm))
        [➜ok (cadr (tape tm))]
        [➜rightmost]
        )))

  (defun-typed w◧ ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (car (tape tm)) instance)
      [➜ok]
      ))

  (defun-typed esw◧ ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (cdr (head tm))
        (progn
          (setf (cadr (tape tm)) instance)
          [➜ok]
          )
        [➜rightmost]
        )))



;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed c◧ ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) (tape tm))
      [➜ok]
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed s ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if 
        (cdr (head tm))
        (progn
          (setf (head tm) (cdr (head tm)))
          [➜ok]
          )
        [➜rightmost]
        )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (let(
            (next-cell (cdr (head tm)))
            )
        (rplacd (head tm) (cons instance next-cell))
        [➜ok]
        )))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defun-typed on-leftmost ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (eq (head tm) (tape tm)) [➜t] [➜∅])
      ))

  (defun-typed on-rightmost ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (¬ (cdr (head tm))) [➜t] [➜∅])
      ))

;;--------------------------------------------------------------------------------
;; length-tape
;;
  (defun-typed tape-length-is-one ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (cdr (tape tm)) [➜t] [➜∅])
      ))

  (defun-typed tape-length-is-two ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if 
        (∧ (cdr (tape tm)) (cddr (tape tm)))
        [➜t] [➜∅]
        )))
      
