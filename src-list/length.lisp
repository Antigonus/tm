#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
|#
(in-package #:tm)

;; (length tm0) <?> ( (length tm1) | n )
(defgeneric length-cmp (tm0 n-or-tm1 &optional cont-longer cont-same cont-shorter))

(defgeneric empty     (tm &optional cont-true cont-false))
(defgeneric singleton (tm &optional cont-true cont-false))
(defgeneric doubleton (tm &optional cont-true cont-false))
(defgeneric tripleton (tm &optional cont-true cont-false))

;; specialized versions might be faster, otherwise would have made these functions.
;;
  (defmethod empty ((tm0 tape-machine) &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore cont-true))
    (funcall cont-false)
    )

  (defmethod singleton ((tm0 tape-machine) &optional (cont-true (be t)) (cont-false (be ∅)))
    (let(
          (tm1 (fork-0 tm0))
          )
      (cue-leftmost tm1)
      (on-rightmost tm1
        cont-true
        cont-false
        )))
  
  (defmethod doubleton ((tm0 tape-machine) &optional (cont-true (be t)) (cont-false (be ∅)))
    (let (
           (tm1 (fork-0 tm0))
           )
      (cue-leftmost tm1)
      (if (∧
            (s tm1)
            (on-rightmost tm1)
            )
      (funcall cont-true)
      (funcall cont-false)
      )))
  
  (defmethod tripleton ((tm0 tape-machine) &optional (cont-true (be t)) (cont-false (be ∅)))
    (let (
           (tm1 (fork-0 tm0))
           )
      (cue-leftmost tm1)
      (if
        (∧
          (s tm1)
          (s tm1)
          (on-rightmost tm1)
          )
      (funcall cont-true)
      (funcall cont-false)
      )))

;; for more than trippleton, use #'cue-leftmost #'length=


;;--------------------------------------------------------------------------------
;; cmp
;;
  ;; (length tm0) <?> n
  (defmethod length-cmp
    (
      (tm0 tape-machine)
      (n integer)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (let ((tm0-1 (fork-0 tm0)))
      (⟳ (λ(cont-loop cont-return) 
           (when (≤ n 0) (return-from length-cmp (funcall cont-longer)))
           (decf n)
           (s tm0-1 cont-loop cont-return)
           ))
      (if 
        (= n 0) 
        (funcall cont-same)
        (funcall cont-shorter)
        )))

  ;; (length tm0) <?> (length tm1)
  (defmethod length-cmp
    (
      (tma tape-machine)
      (tmb tape-machine)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (let(
          (tma-1 (fork-0 tma))
          (tmb-1 (fork-0 tmb))
          )
      (let(
            (tms (mount {tma-1 tmb-1}))
            )
        (⟳ (λ(cont-loop cont-return)
             (let(
                   (on-rm-a (on-rightmost tma-1))
                   (on-rm-b (on-rightmost tmb-1))
                   )
               (cond
                 ((∧ on-rm-a on-rm-b)
                   (return-from length-cmp (funcall cont-same)))
                 (on-rm-b
                   (return-from length-cmp (funcall cont-longer)))
                 (on-rm-a
                   (return-from length-cmp (funcall cont-shorter))
                   )
                 ))
             (s-together tms cont-loop cont-return)
             ))
        (error 'tm-impossible-to-get-here :text "length-cmp should not be able to get here")
        )))

;;--------------------------------------------------------------------------------
;; all possible reduction to a Boolean operator, excluding constants true and false.
;;

  ;;000 false

  ;;001
  (defun length< 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-false cont-true)
    )

  ;;010
  (defun length= 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-true cont-false)
    )

  ;;011
  (defun length≤ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-true cont-true)
    )

  ;;100
  (defun length> 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-false cont-false)
    )

  ;;101
  (defun length≠ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-false cont-true)
    )

  ;;110
  (defun length≥
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-true cont-false)
    )

   ;;111 true

