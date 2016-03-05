#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
  Bounded length checks.

|#
(in-package #:tm)


(defgeneric singleton (tm))
(defgeneric doubleton (tm))
(defgeneric tripleton (tm))

;; (length tm0) <?> ( (length tm1) | n )
(defgeneric length-cmp (tm0 n-or-tm1 &optional cont-longer cont-same cont-shorter))


;; specialized versions might be faster, otherwise would have made these functions.
;;
  (defmethod singleton ((tm0 tape-machine))
    (and
      (on-leftmost tm0)
      (on-rightmost tm0)
      ))
  
  (defmethod doubleton ((tm0 tape-machine))
    (let (
           (tm1 (dup tm0))
           )
      (cue-leftmost tm1)
      (and
        (s tm1)
        (on-rightmost tm1)
        )))
  
  (defmethod tripleton ((tm0 tape-machine))
    (let (
           (tm1 (dup tm0))
           )
      (cue-leftmost tm1)
      (and
        (s tm1)
        (s tm1)
        (on-rightmost tm1)
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
    (let ((tm0-1 (dup tm0)))
      (⟳ tm0-1 #'s
        (λ() 
          (when (≤ n 0) (return-from length-cmp (funcall cont-longer)))
          (decf n)
          )
        (λ() 
          (if 
            (= n 0) 
            (funcall cont-same)
            (funcall cont-shorter)
            )))))

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
          (tma-1 (dup tma))
          (tmb-1 (dup tmb))
          )
      (let(
            (tms (mk-tm (list tma-1 tmb-1)))
            )
        (⟳ tms #'s-together
          (λ() 
            (let(
                  (on-rm-a (on-rightmost tma-1))
                  (on-rm-b (on-rightmost tmb-1))
                  )
              (cond
                ((and on-rm-a on-rm-b)
                  (return-from length-cmp (funcall cont-same)))
                (on-rm-b
                  (return-from length-cmp (funcall cont-longer)))
                (on-rm-a
                  (return-from length-cmp (funcall cont-shorter))
                  )
                )))
          (λ()
            (error 'tm-impossible-to-get-here :text "length-cmp should not be able to get here")
            )))))


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

