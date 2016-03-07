#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
  Info about where the head is located within the tape's address space.

  Array specializations will have more efficient implementation than these
  generic ones.

  If you have a good idea which tape machine is on the left, it will be 
  more efficient to provide that one as the first argument.

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; absolute location
;;

  (defgeneric on+1 (tm &optional cont-true cont-false)
    (:documentation
      "tm head is one to the right of the leftmost cell, i.e. on cell 1."
      ))

  (defmethod on+1
    ( 
      (tm0 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (tm1 (dup tm0))
          )
      (cue-leftmost tm1)
      (s tm1
        (λ() (heads-on-same-cell tm0 tm1 cont-true cont-false))
        cont-false
        )))


  (defgeneric on+n (tm n &optional cont-true cont-false)
    (:documentation
      "tm head is on cell n."
      ))

  (defmethod on+n
    ( 
      (tm0 tape-machine) 
      (n integer)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (cond
      ((< n 0) (funcall cont-false)) ; the head is never left of leftmost !
      ((= n 0) (on-leftmost tm0 cont-true cont-false))
      (t
        (let(
              (tm1 (dup tm0))
              )
          (cue-leftmost tm1)
          (sn tm1 n
            (heads-on-same-cell tm0 tm1 cont-true cont-false)
            cont-false
            )))
      ))

;; on rightmost -1  (s tm) would be on rightmost
  (defgeneric on-rightmost-1 (tm &optional cont-true cont-false))

  ;; -1
  (defmethod on-rightmost-1
    ( 
      (tm0 tape-machine) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (tm1 (dup tm0))
          )
      (s tm1)
      (on-rightmost tm1 cont-true cont-false)
      ))

  ;; n will typically be negative
  (defgeneric on-rightmost-n (tm n &optional cont-true cont-false))

  (defmethod on-rightmost-n
    ( 
      (tm0 tape-machine) 
      (n integer)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (cond
      ((> n 0) (funcall cont-false)) ; the head is never right of rightmost
      ((= n 0) (on-rightmost tm0 cont-true cont-false))
      (t
        (let(
              (tm1 (dup tm0))
              )
          (sn tm1 n
            (on-rightmost tm1 cont-true cont-false)
            cont-false
            )))
      ))
   

  ;; assigns a natural number to each head location
  (defgeneric address (tm)
    (:documentation
      "address of head.  Address is zero when the head is on leftmost."
      ))

  (defmethod address ( (tm0 tape-machine) )
    (let(
          (n 0)
          (tm1 (dup tm0))
          )
      (cue-leftmost tm1)
      (labels(
               (take-step()
                 (s≠ 
                   tm1
                   tm0 
                   (λ()(incf n)(take-step))
                   (λ() n)
                   ))
               )
        (take-step)
        )))

;;--------------------------------------------------------------------------------
;; relative location
;;
  (defgeneric distance-1 (tm0 tm1 &optional cont-true cont-false))

  (defmethod distance-1
    ( 
      (tm0 tape-machine) 
      (tm1 tape-machine)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (tm0-dup (dup tm0))
          )
      (s tm0-dup
        (λ() (heads-on-same-cell tm0-dup tm0 cont-true cont-false))
        cont-false
        )))

  (defgeneric distance-n (tm0 tm1 n &optional cont-true cont-false))

  (defmethod distance-n
    ( 
      (tm0 tape-machine) 
      (tm1 tape-machine)
      (n integer)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (cond
      ((< n 0)
        (distance-n tm0 tm1 (- n) cont-true cont-false)
        )
      ((= n 0)
        (heads-on-same-cell tm0 tm1 cont-true cont-false)
        )
      (t
        (let(
              (tm0-dup (dup tm0))
              )
          (sn tm0-dup n
            (λ() (heads-on-same-cell tm0-dup tm1 cont-true cont-false))
            cont-false
            )))))


;;--------------------------------------------------------------------------------
;; compare locations
;;

  ;; (location tm0) <?> ( (location tm1) | n )
  (defgeneric location-cmp (tm0 n-or-tm1 &optional cont-longer cont-same cont-shorter))


  ;; (location tm0) <?> n
  (defmethod location-cmp
    (
      (tm0 tape-machine)
      (n1 integer)
      &optional 
      (cont-right-of (be 'right-of))
      (cont-same     (be 'same))
      (cont-left-of  (be 'left-of))
      )
    (let(
          (n0 0)
          (tm0-dup (dup tm0))
          )
      (cue-leftmost tm0-dup)
      (labels(
               (take-step()
                 (s≠
                   tm0-dup
                   tm0
                   (λ()
                     (incf n0)
                     (if 
                       (> n0 n1) 
                       (funcall cont-right-of)
                       (take-step)
                       ))
                   (λ()
                     (if 
                       (< n0 n1) 
                       (funcall cont-left-of)
                       (funcall cont-same)
                       ))))
               )
        (take-step)
        )))

  ;; (location tm0) <?> (location tm1)
  (defmethod location-cmp
    (
      (tm0 tape-machine)
      (tm1 tape-machine)
      &optional 
      (cont-right-of (be 'right-of))
      (cont-same   (be 'same))
      (cont-left-of (be 'left-of))
      )
    (if
      (heads-on-same-cell tm0 tm1)
      (funcall cont-same)
      (let(
            (tm0-dup (dup tm0))
            )
        (cue-leftmost tm0-dup)
        (labels(
                 (take-step()
                   (s≠
                     tm0-dup
                     tm0
                     (λ()
                       (if
                         (heads-on-same-cell tm0-dup tm1) ; but tm0 isn't on the same cell..
                         (funcall cont-right-of)
                         (take-step)
                         ))
                     (λ()
                       (funcall cont-left-of)
                       )))
                 )
          (take-step)
          ))))


;;--------------------------------------------------------------------------------
;; all possible reduction to a Boolean operator, excluding constants true and false.
;;

  ;;000 false

  ;;001
  (defun location< 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-false cont-false cont-true)
    )

  ;;010
  (defun location= 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-false cont-true cont-false)
    )

  ;;011
  (defun location≤ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-false cont-true cont-true)
    )

  ;;100
  (defun location> 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-true cont-false cont-false)
    )

  ;;101
  (defun location≠ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-true cont-false cont-true)
    )

  ;;110
  (defun location≥
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (location-cmp tm0 n-or-tm1 cont-true cont-true cont-false)
    )

   ;;111 true

