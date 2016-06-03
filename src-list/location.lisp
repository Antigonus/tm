#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
  Info about where the head is located within the tape's address space.

  See tm-derived.lisp for on-leftmost and on-rigthmost

  Array specializations will have more efficient implementation than these
  generic ones.

  By the term 'leftmost'  we refer to the only cell on a projective tape that
  does not have a left neighbor.  Leftmost has address zero.  The right neighbor
  of leftmost has address 1, etc.  

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
          (tm1 (fork-0 tm0))
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
              (tm1 (fork-0 tm0))
              )
          (cue-leftmost tm1)
          (sn tm1 n
            (λ()(heads-on-same-cell tm0 tm1 cont-true cont-false))
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
          (tm1 (fork-0 tm0))
          )
      (s tm1)
      (on-rightmost tm1 cont-true cont-false)
      ))

  ;; n will typically be negative
  (defgeneric on-rightmost+n (tm n &optional cont-true cont-false))

  (defmethod on-rightmost+n
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
              (tm1 (fork-0 tm0))
              )
          (sn tm1 (- n)
            (λ()(on-rightmost tm1 cont-true cont-false))
            cont-false
            )))
      ))
   

  ;; assigns a natural number to each head location
  (defgeneric address (tm)
    (:documentation
      "address of head.  Address is zero when the head is on leftmost."
      ))

  ;; specialized version might be a lot faster
  (defmethod address ((tm0 tape-machine))
    (let(
          (tm1 (mount tm0))
          (address 0)
          )
      (⟳(λ(cont-loop cont-return)
          (heads-on-same-cell tm1 tm0
            cont-return
            (λ() 
              (s tm1
                (λ()(incf address) (funcall cont-loop))
                #'cant-happen
                )))))
      address
      ))

;;--------------------------------------------------------------------------------
;; relative location
;;
  (defgeneric distance+1 (tm0 tm1 &optional cont-true cont-false)
    (:documentation
      "The tm1 is one step to the right of tm0."
      ))

  (defmethod distance+1
    ( 
      (tm0 tape-machine) 
      (tm1 tape-machine)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (let(
          (fk0 (fork-0 tm0))
          )
      (s fk0
        (λ() (heads-on-same-cell fk0 tm1 cont-true cont-false))
        cont-false
        )))

  (defgeneric distance+n (tm0 tm1 n &optional cont-true cont-false)
    (:documentation
      "tm1 is n steps to the right of tm0."
      ))

  (defmethod distance+n
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
        (distance+n tm1 tm0 (- n) cont-true cont-false)
        )
      ((= n 0)
        (heads-on-same-cell tm0 tm1 cont-true cont-false)
        )
      (t
        (let(
              (fk0 (fork-0 tm0))
              )
          (sn fk0 n
            (λ() (heads-on-same-cell fk0 tm1 cont-true cont-false))
            cont-false
            )))))

  ;; distance from tm0 on the left, to tm1 on the right
  (defgeneric distance (tm0 tm1)
    (:documentation
      "Returns the number of steps needed by tm0 to reach tm1, might be negative."
      ))

  ;; not a good implementation ..
  ;; this should calc steps from one to the other as address is open ended
  ;; it would have to interleave going both directions
  ;; keep this as a reference implementation.
  (defmethod distance
    ( 
      (tm0 tape-machine) 
      (tm1 tape-machine)
      )
    (- (address tm1) (address tm0))
    )

;;--------------------------------------------------------------------------------
;; compare locations
;;

  ;; (location tm0) <?> ( (location tm1) | n )
  (defgeneric location-cmp (tm0 n-or-tm1 &optional cont-longer cont-same cont-shorter))


  ;; not good implementations
  ;; both location-cmps need to short circuit, stop stepping, once the result is
  ;; apparent .. had code doing that before, but I changed some interface defs..
  ;; keep these as reference implementations, (a level above tape machine??)

  ;; (location tm0) <?> n
  (defmethod location-cmp
    (
      (tm0 tape-machine)
      (n1 integer)
      &optional 
      (cont-left-of  (be 'left-of))
      (cont-same     (be 'same))
      (cont-right-of (be 'right-of))
      )
    (let(
          (n0 (address tm0))
          )
      (cond
        ((< n0 n1) (funcall cont-left-of))
        ((= n0 n1) (funcall cont-same))
        ((> n0 n1) (funcall cont-right-of))
        )))

  ;; (location tm0) <?> (location tm1)
  (defmethod location-cmp
    (
      (tm0 tape-machine)
      (tm1 tape-machine)
      &optional 
      (cont-left-of (be 'left-of))
      (cont-same   (be 'same))
      (cont-right-of (be 'right-of))
      )
    (location-cmp tm0 (address tm1) cont-left-of cont-same cont-right-of)
    )


;;--------------------------------------------------------------------------------
;; all possible reduction to a Boolean operator, excluding constants true and false.
;;

  ;;000 false

  ;;001
  (defun location>
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
  (defun location≥ 
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
  (defun location< 
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
  (defun location≤
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

