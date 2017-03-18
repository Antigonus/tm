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

  (defun-typed ec◧r ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok (binode-instance (tape tm))]
      ))

  (defun-typed ec◧sr ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (if
        (binode-right-neighbor (tape tm))
        [➜ok (binode-instance (binode-right-neighbor (tape tm)))]
        [➜rightmost]
        )))

  (defun-typed ec◧w ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (binode-instance (tape tm)) instance)
      [➜ok]
      ))

  (defun-typed ec◧sw ((tm bilist-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (binode-right-neighbor (tape tm))
        (progn
          (setf (binode-instance (binode-right-neighbor (tape tm))) instance)
          [➜ok]
          )
        [➜rightmost]
        )))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  ;; c◧ inherited from list-tm

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
        (binode-right-neighbor (head tm))
        (progn
          (setf (head tm) (binode-right-neighbor (head tm)))
          [➜ok]
          )
        [➜rightmost]
        )))

  (defun-typed -s ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if 
        (binode-left-neighbor (head tm))
        (progn
          (setf (head tm) (binode-left-neighbor (head tm)))
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
            (node (head tm))
            (current-right-neighbor (binode-right-neighbor (head tm)))
            (new-right-neighbor (make-binode))
            )
        (setf (binode-instance       new-right-neighbor) instance)
        (setf (binode-left-neighbor  new-right-neighbor) node)
        (setf (binode-right-neighbor new-right-neighbor) current-right-neighbor)
        (when current-right-neighbor
          (setf (binode-left-neighbor current-right-neighbor) new-right-neighbor)
          )
        (setf (binode-right-neighbor node) new-right-neighbor)
        [➜ok]
        )))

;;--------------------------------------------------------------------------------
;; location
;;  
  ;; on-leftmost inherited from list-tm

  (defun-typed on-rightmost ((tm bilist-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (binode-right-neighbor (head tm)) [➜∅] [➜t])
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
      (if (binode-right-neighbor (tape tm)) [➜∅] [➜t])
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
        (∧
          (binode-right-neighbor (tape tm))
          (¬ (binode-right-neighbor (binode-right-neighbor (tape tm))))
          )
        [➜t] [➜∅]
        )))
      
