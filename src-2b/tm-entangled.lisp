#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A reference to a tape machine that also specifies which head to use.  
  A head reference is an index into the head tape array. 

  As this reference keeps a pointer to the base tape machine, the base
  tape machine will not be gc'd underneath it.  However, when this reference
  is abandoned, the base tape machine will not know that the corresponding
  head has been released, unless it is explicitly marked.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-entangled (tm)
    (
      (base
        :initarg :base
        :accessor base
        )
      (head-number
        :initarg :head
        :accessor head
        )
      ))

  (defun-typed head ((tm tm-entangled))
    (r<tape-array> (head (base tm)) {:address (head-number tm)})
    )

  (defun-typed init ((tm tm-entangled) (base tm) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (setf (base tm) base)
      (max<tape-array> (head base)
        {
          :➜ok (λ(max)
                 (setf (head-number tm) (1+ max))
                 )
          :➜empty #'cant-happen ; any tm made with #'init will have at least one head
          })))


;;--------------------------------------------------------------------------------
;; 
  (def-function-class are-entangled (tm0 tm1 &optional ➜))

  (defun-typed are-entangled ((tm0 tm-entangled) tm1 &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if (eq (base tm0) tm1) [➜t] [➜∅])
      ))

  (defun-typed are-entangled (tm0 (tm1 tm-entangled)  &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if (eq tm0 (base tm1)) [➜t] [➜∅])
      ))

  (defun-typed are-entangled ((tm0 tm-entangled) (tm1 tm-entangled)  &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if (eq (base tm0) (base tm1)) [➜t] [➜∅])
      ))

  (def-function-class tm= ((tm0 tm) (tm1 tm))
    (destructuring-bind
      (
        &key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (are-entangled tm0 tm1
        {
          :➜∅ ➜∅
          :➜t (λ()(if (eq (head tm0) (head tm1)) [➜t] [➜∅]))
          })))

;;--------------------------------------------------------------------------------
;; absolue head control
;;
  (defun-typed p ((tm tm-entangled) &optional ➜)
    (p (base tm) {:head (head-number tm) (o ➜)})
    )

  ;; cue the head
  (defun-typed u ((tm tm-entangled) &optional ➜)
    (u (base tm) {:head (head-number tm) (o ➜)})
    )

  ;; head location
  (defun-typed @ ((tm tm-entangled) &optional ➜)
    (@ (base tm) {:head (head-number tm) (o ➜)})
    )

;;--------------------------------------------------------------------------------
;; relative head control
;;
  (defun-typed s ((tm tm-entangled) &optional ➜)
    (s (base tm) {:head (head-number tm) (o ➜)})
    )
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (defun-typed r ((tm tm-active) &optional ➜)
    (r (base tm) {:head (head-number tm) (o ➜)})
    )

  (defun-typed w ((tm tm-active) instance &optional ➜)
    (w (base tm) instance {:head (head-number tm) (o ➜)})
    )
   

;; ..
;; destructive operations might take ➜collision continuations
