#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  tm-ref-array-realloc - tape machine based on a tape which is a tape-ref-array-realloc

  tm-ref-array-realloc-chasis holds the tape and a list of tm-ref-array-realloc instances.  In turn
  each tm-ref-array-realloc instance has a head and a references the chasis.  Thus the chasis and the
  tem are mutually referencial. I use weak pointers so that the gc can deal with tihs.

  With this approach we can avoid having to keep entanglement lists.  Because each tm has
  access to the chasis, it can automatically communicate with all tms.  So for example, if
  one goes empty, then that one can make all the others empty.  This implimentation is
  more compact than the prior entanglment list model (where each tm shared a listener
  list) thought the prior entanglement list approach did better model distributed computing
  with shared memory. Perhaps for this reason it might come back some day.

  Wish the head could be a pointer into the array rather than an index, as the repeated
  access calculation is a bit of a waste.  But we can't do this because on an expanstion
  event the ref-array-realloc can change out the array.  That is why its interface is a
  bunch of macros.  Accordingly, indexes remain valid, but pointers would not.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-ref-array-realloc (tm)
    (
      (chasis :accessor chasis)
      (head :accessor head)
      ))

  (def-type tm-ref-array-realloc-abandoned (tm-ref-array-realloc tm-abandoned)()) ; used by scoping operators

  ;; useful conjunctions of status:
  (def-type tm-ref-array-realloc-empty-parked-active (tm-ref-array-realloc tm-empty-parked-active)()) ; not abandoned
  (def-type tm-ref-array-realloc-empty-parked        (tm-ref-array-realloc tm-empty-parked)()) ; not active
  (def-type tm-ref-array-realloc-parked-active       (tm-ref-array-realloc tm-parked-active)()) ; not empty

  (def-type tm-ref-array-realloc-empty
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-empty-parked
      tm-empty
      )
    ()
    )
  (def-type tm-ref-array-realloc-parked
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-empty-parked
      tm-ref-array-realloc-parked-active
      tm-parked
      )
    ()
    )
  (def-type tm-ref-array-realloc-active
    (
      tm-ref-array-realloc-empty-parked-active
      tm-ref-array-realloc-parked-active
      tm-active
      )
    ()
    )

  (defun-typed to-abandoned ((tm tm-ref-array-realloc))
    (setf (chasis tm) ∅) ; so that a bunch of data doesn't stay alive while the tm is in limbo
    (change-class tm 'tm-ref-array-realloc-abandoned)
    )
  (defun-typed to-empty     ((tm tm-ref-array-realloc))
    (setf (head tm) 'enpty) ; to prevent unintended communication on using an empty machine
    (change-class tm 'tm-ref-array-realloc-empty)
    )
  (defun-typed to-parked    ((tm tm-ref-array-realloc))
    (setf (head tm) 'parked) ; to prevent unintended communication on a parked  machine
    (change-class tm 'tm-ref-array-realloc-parked)
    )
  (defun-typed to-active  ((tm tm-ref-array-realloc)) 
    ;; external code sets the head before calling this, the head must have an array index when active
    (change-class tm 'tm-ref-array-realloc-active)
    )

  (def-type chasis-array ()
    (
      (heap  ;; intended to be the heap that holds the tms.  Currently we emulate the heap.
        :initform ∅
        :accessor heap
        )
      (tape
        :initform ∅
        :accessor tape
        )
      ))

;;--------------------------------------------------------------------------------
;; heap emulation
;;
;;  These are not available on the library interface, rather are used internally.
;;
;;  we use a linked list for the heap.  Instead of instances being the heap, the
;;  instances is passed in, and a reference to it is kept on the heap.  We hook
;;  into the GC system by using weak pointers.
;;
  (defun loc-tm-alloc (tm chasis) ; normally tm would not be an operand to alloc
    (let(
          (heap (heap chasis))
          (tm-pointer (tg:make-weak-pointer tm))
          )
      (if (¬ heap)
        (setf (chasis heap) (cons tm-pointer ∅))
        (⟳(λ(➜again)
            (if (¬ (tg:weak-pointer-value (car heap)))
              (setf (car heap) tm-pointer)
              (if (¬ (cdr heap))
                (setf (cdr heap) (cons tm-pointer ∅))
                (progn
                  (setf heap (cdr heap))
                  [➜again]
                  )))))))
    tm
    )

  ;; left out the 'hey this aint on the heap in the first place' error for now.
  ;; convnetional delete
  ;;
    (defun loc-tm-dealloc (tm chasis)
      (let(
            (pt (heap chasis))
            )
        (when pt ; i.e. when not empty list
          (if (eq (tg:weak-pointer-value (car pt)) tm)
            (setf (heap chasis) (cdr pt))
            (⟳(λ(➜again)
                (when (cdr pt)
                  (if (eq (tg:weak-pointer-value (cadr pt)) tm)
                    (setf (cdr pt) (cddr pt))
                    (progn
                      (setf pt (cdr pt))
                      [➜again]
                      )))))))))

  ;; for reference, a right only delete implementation
  #|
    (defun tm-dealloc (tm chasis)
      (cond
        ((empty-list (heap chasis)) t)
        ((singleton (heap chasis)) (make-empty-list (heap chasis)))
        (t
          (let(
                (pt (heap chasis))
                )
            (⟳(λ(➜again)
                (if (eq (tg:weak-pointer-value (car pt)) tm)
                  (progn
                    (when (is-rightmost pt) (setf (heap chasis) pt))
                    (setf (car pt) (cadr pt))
                    (setf (cdr pt) (cddr pt))
                    )
                  (unless (is-rightmost pt)
                    (setf pt (cdr pt))
                    [➜again]
                    ))))
            ))))
  |#

;;--------------------------------------------------------------------------------
;; absolue head control
;;

  ;; head address
  ;; for a multidimensional base array the address will be a list
  (defun-typed @ ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (head tm)] ; tape array head is the location
      ))

  ;; cue the head
  ;; need to add a past right bound continuation to this
  (defun-typed u ((tm tm-ref-array-realloc-parked) &optional ➜)
    (to-active tm)
    (u tm ➜)
    )

  ;; we really need to inline and optimize calls to this ..
  ;; need to add a past right bound continuation to this
  ;; if we attempt to cue off the tape, the head is not moved, and a spill value (how far out of 
  ;; the region) is returned with the bound continuation 
  (defun-typed u ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) 
        bound-address
        bound-tm 
        (➜ok (be t))
        (➜bound (be ∅)) ; takes one operand, the spill amount
        &allow-other-keys
        )
      ➜
      (let(
            max
            )
        ;; user probably shouldn't set both the bound-address and bound-tm .. but just in case
        ;; take the lesser of the bound address and address of the bound-tm
        (when (∧ bound-tm bound-address (< (@ bound-tm) bound-address)) (setf bound-address (@ bound-tm)))
        (cond
          (bound-address (setf max bound-address))
          (bound-tm (setf max (@ bound-tm))) ; we only get here if there is not a bound-address
          (t
            (setf max (max<tape-ref-array-realloc> (tape (chasis tm))))
            ))
        (cond
          ((< address 0) 
            [➜bound address]
            )
          ((> address max)
            [➜bound (- address max)]
            )
          (t
            (setf (head tm) address)
            [➜ok]
            )))))


;;--------------------------------------------------------------------------------
;; relative head control
;;

  ;; a right step from parked is leftmost
  ;; a left step from parked is rightmost
  ;; usually Δ is a constant (but for all invocations?), so I hope that the optimizing compiler reduces this code
  ;; want to add functioning inlining for step and queue .. can we do that with dispatched functions? probably not.
  (defun-typed s ((tm tm-ref-array-realloc-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (cond
        ((= Δ 0)
          [➜ok]
          )
        ((> Δ 0)
          (u tm {:address (1- Δ) (o ➜)})
          )
        ((< Δ 0)
          (let(
                (max (max<tape-ref-array-realloc> (tape (chasis tm))))
                )
            (u tm {:address (- max (+ Δ 1)) (o ➜)})
            ))
        (t #'cant-happen)
        )))
  (defun-typed s ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (Δ 1)
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (cond
        ((= Δ 0)
          [➜ok]
          )
        (t
          (let(
                (new-address (+ (head tm) Δ))
                )
            (u tm {:address new-address (o ➜)})
            )))))

  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (defun-typed r ((tm tm-ref-array-realloc-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address (head tm))
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (r<tape-ref-array-realloc> (tape (chasis tm))
        {
          :address address
          :➜ok ➜ok
          :➜empty #'cant-happen
          })))

  (defun-typed r ((tm tm-ref-array-realloc-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (address ∅)
        (➜ok #'echo)
        (➜parked #'accessed-parked)
        &allow-other-keys
        )
      ➜
      (if
        address
        (r<tape-ref-array-realloc> (tape (chasis tm))
          {
            :address address
            :➜ok ➜ok
            })
        [➜parked]
        )))

  (defun-typed w ((tm tm-ref-array-realloc-active) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address (head tm))
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (w<tape-ref-array-realloc> (tape (chasis tm)) instance
        {
          :address address
          :➜ok ➜ok
          :➜alloc-fail ➜alloc-fail
          })))

  (defun-typed w ((tm tm-ref-array-realloc-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address ∅)
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (if
        address
        (progn
          (w<tape-ref-array-realloc> (tape (chasis tm)) instance
            {
              :address address
              :➜ok ➜ok
              :➜alloc-fail ➜alloc-fail
              })
          (to-parked tm)
          )
        [➜empty]
        )))

  (defun-typed w ((tm tm-ref-array-realloc-parked) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (address ∅)
        (➜ok (be t))
        (➜alloc-fail #'alloc-fail)
        (➜parked #'accessed-parked)
        &allow-other-keys
        )
      ➜
      (if
        address
        (w<tape-ref-array-realloc> (tape (chasis tm)) instance
          {
            :address address
            :➜ok ➜ok
            :➜alloc-fail ➜alloc-fail
            })
        [➜parked]
        )))



;;--------------------------------------------------------------------------------
;; instance creation
;;
  (defun-typed init ((tm tm-ref-array-realloc) value &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        (➜bad-init (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (let(
            (chasis (make-instance 'chasis-array))
            )
        (setf (chasis tm) chasis)
        (w<tape-ref-array-realloc> (heap chasis) (tg:make-weak-pointer tm))
        (cond
          ((typep value 'null)
            (to-empty tm)
            [➜ok tm]
            )
          ((typep value 'box) ; unboxed value must be a tape-ref-array-realloc type instance!
            (setf (tape chasis) (unbox value))
            (to-parked tm)
            [➜ok tm]
            )
          (t
            [➜bad-init]
            )))))

  (defun-typed init ((tm1 tm-ref-array-realloc) (tm0 tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (chasis (chasis tm0))
            )
        (loc-tm-alloc tm1 chasis)
        (setf (chasis tm1) chasis)
        (setf (head tm1) (head tm0)) ; starts in the same place
        [➜ok tm1]
        )))

  (defun-typed disentangle ((tm tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (to-abandoned tm)
      (loc-tm-dealloc tm)
      ))


#|



  (defun-typed fork ((tm tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (let(
            (tape (tape (chasis tm)))
            (new-tape ∅)
            )
        (let(
              (i (max<tape-ref-array-realloc> tape))
              )
          (⟳(λ(➜again) ; important to write max address first, so that the new-tape doesn't repeatedly expand
              (w<tape-ref-array-realloc> new-tape (r<tape-ref-array-realloc> tape {:address i}))
              (when (> i 0)
                (decf i)
                [➜again]
                ))))
        (let(
              (new-chasis (make-instance 'chasis))
              (new-tm (make-instance 'tm))
              )
          (setf (tape new-chasis) new-tape)
          (setf (chasis new-tm) new-chasis)
          (setf (head new-tm) (head tm))
          (a◨<tape-ref-array-realloc> (heap new-chasis) (tg:make-weak-pointer new-tm))
          [➜ok new-tm]
          ))
      ))
|#

