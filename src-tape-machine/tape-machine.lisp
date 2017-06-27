#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Architectural definition of Tape Machine.
I.e. the tape machine interface.

Abandoned is the state an object is in from the time it is marked as no longer being used
by the program, until the garbage collector picks it up.  No operations are defined for
abandoned machines.

The architectural tape machine does not presume what the implementation wil be, hence it
has no slots.  Of course this means we can not assume that tape-machine has a head or a
tape. We provide a generic tape machine implementation in 'tm.lisp', that provides
a generic implementation of a tape machine built only over the tape interface.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; helpers
;;
  (defmacro def-empty-1 (f &rest args)
    `(defun-typed ,f ((tm tape-machine-empty) ,@args &optional ➜)
       (declare (ignore ,@args))
       (destructuring-bind
         (
           &key
           (➜empty #'accessed-empty)
           &allow-other-keys
           )
         ➜
         [➜empty]
         ))
    )

  (defmacro def-parked-1 (f &rest args)
    `(defun-typed ,f ((tm tape-machine-parked) ,@args &optional ➜)
       (declare (ignore ,@args))
       (destructuring-bind
         (
           &key
           (➜parked #'access-through-parked-head)
           &allow-other-keys
           )
         ➜
         [➜parked]
         ))
    )

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tape-machine ()())

  (def-type tape-machine-abandoned (tape-machine)())

  (defun-typed tape-machine-empty-or-parked (tape-machine)())
  (defun-typed tape-machine-parked-or-active (tape-machine)())

  (def-type tape-machine-empty (tape-machine-empty-or-parked)())
  (def-type tape-machine-parked
    (tape-machine-empty-or-parked tape-machine-parked-or-active)
    ()
    )
  (def-type tape-machine-active (tape-machine-parked-or-active)())


  (defun-typed to-abandoned ((tm tape-machine)) (change-class tm 'tape-machine-abandoned))
  (defun-typed to-active    ((tm tape-machine)) (change-class tm 'tape-machine-active))
  (defun-typed to-empty     ((tm tape-machine)) (change-class tm 'tape-machine-empty))
  (defun-typed to-parked    ((tm tape-machine)) (change-class tm 'tape-machine-parked))

  ;; binding directly to sequences
  ;; 
    (defun-typed init ((tm tm) (init cons) &optional ➜)
      (let(
            (tape (mk 'tape-cons init ➜))
            )
        (init tm tape ➜)
        ))
    (defun-typed init ((tm tm) (init sequence) &optional ➜)
      (let(
            (tape (mk 'tape-sequence init ➜))
            )
        (init tm tape ➜)
        ))


;;--------------------------------------------------------------------------------
;; entanglement
;;
  ;; returns an entangled machine
  (def-function-class entangle (tm0 &optional ➜))

  (defun with-entangled (tm continuation &optional ➜)
    (let(
          (etm (entangle tm))
          )
      [continuation etm]
      (to-abandoned etm) ; when we have entanglement accounting this will free etm0 from the listener list
      ))

  ;; predicate tells if two generic machines are entangled
  (def-function-class entangled (tm0 tm1 &optional ➜))


;;--------------------------------------------------------------------------------
;; location
;;
  (def-function-class on-leftmost (tm &optional ➜)
    (:documentation
      "tm head is on leftmost ➜t, else ➜∅
      "))
  (defun-typed on-leftmost ((tm tape-machine-empty-or-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (def-function-class on-rightmost (tm &optional ➜)
    (:documentation
      "tm head is on the rightmost cell ➜t, else ➜∅
      "))
  (defun-typed on-rightmost ((tm tape-machine-empty-or-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  ;; by contract, tm0 and tm1 must be entangled
  ;;
    (def-function-class heads-on-same-cell (tm0 tm1 &optional ➜))
    (defun-typed heads-on-same-cell
      (
        (tm0 tape-machine-parked) 
        (tm1 tape-machine-parked)
        &optional ➜
        )
      (destructuring-bind
        (&key
          (➜t (be t))
          &allow-other-keys
          )
        ➜
        [➜t]
      ))
    (defun-typed heads-on-same-cell
      (
        (tm0 tape-machine-empty-or-parked) 
        (tm1 tape-machine)
        &optional ➜
        )
      (destructuring-bind
        (&key
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        [➜∅]
      ))
    (defun-typed heads-on-same-cell
      (
        (tm0 tape-machine) 
        (tm1 tape-machine-empty-or-parked)
        &optional ➜
        )
      (destructuring-bind
        (&key
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        [➜∅]
      ))

    (def-function-class tm= (tm0 tm1 &optional ➜))
    (deffun-typed tm= ((tm0 tape-machine) (tm1 tape-machine) &optional ➜)
      (destructuring-bind
        (&key
          (➜∅ (be ∅))
          &allow-other-keys
          )
        (entangled tm0 tm1
          {
            :➜∅ ➜∅
            :➜t (heads-on-same-cell tm0 tm1 ➜)
            })))


;;--------------------------------------------------------------------------------
;; length
;;
  (def-function-class tape-length-is-one (tm &optional ➜))
  (defun-typed tape-length-is-one ((tm tape-machine-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (def-function-class tape-length-is-two (tm &optional ➜))
  (defun-typed tape-length-is-two ((tm tape-machine-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (def-function-class r (tm &optional ➜))
  (def-empty-1 r)
  (def-parked-1 r)

   ;; read esr like a program.  First we execute #'e.  then we execute #'s, and
   ;; it takes a ➜rightmost error.  Hence, we never get to the read.
  (def-function-class esr (tm &optional ➜))
  (defun-typed esr ((tm tape-machine-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed esr ((tm tape-machine-parked) &optional ➜) (◧r tm ➜))
    
  (def-function-class esnr (tm n &optional ➜))
  (defun-typed esnr ((tm tape-machine-empty) n &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (if (= 0 n)
        [➜empty]
        [➜rightmost]
        )))

  ;; see tape:  (def-function-class ◧r (tm &optional ➜))
  (def-empty-1 ◧r)

  ;; see tape: (def-function-class ◧sr (tm &optional ➜))
  (def-empty-1 ◧sr)

  ;; see tape: (def-function-class ◧snr (tm n &optional ➜))
  (def-empty-1 ◧snr)

  (defun-typed ◨r ((tm tape-machine-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed ◨-sr ((tm tape-machine-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; see tape: (def-function-class w (tm instance &optional ➜))
  (def-empty-1 w instance)
  (def-parked-1 w instance)

  ;; see tape: (def-function-class esw (tm instance &optional ➜))
  ;; and empty machine has a parked head
  (defun-typed esw ((tm tape-machine-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed esw ((tm tape-machine-parked) instance &optional ➜) (◧w tm instance ➜))

  ;; see tape: (def-function-class esnw (tm n instance &optional ➜))
  (defun-typed esnw ((tm tape-machine-empty) n instance &optional ➜)
    (declare (ignore tm n instance))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= 0 n)
        [➜empty]
        [➜rightmost]
        )))

  ;; see tape: (def-function-class ◧w (tm instance &optional ➜))
  (defun-typed ◧w ((tm tape-machine-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; see tape: (def-function-class ◧sw (tm instance &optional ➜))
  (defun-typed ◧sw ((tm tape-machine-empty) instance &optional ➜)
    (declare (ignore tm instance))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  ;; see tape: (def-function-class ◧snw (tm n instance &optional ➜))
  (defun-typed ◧snw ((tm tape-machine-empty) instance &optional ➜)
    (declare (ignore tm instance))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed ◨w ((tm tape-machine-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed ◨-sw ((tm tape-machine-empty) instance &optional ➜)
    (declare (ignore tm instance))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

;;--------------------------------------------------------------------------------
;; head motion
;;
  (def-function-class s (tm &optional ➜)
    (:documentation
      "If the head is on a cell, and there is a right neighbor, puts the head on the
       right neighbor and ➜ok.  If there is no right neighbor, then ➜rightmost.
       "))
  (defun-typed s ((tm tape-machine-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))
  (defun-typed s ((tm tape-machine-parked) &optional ➜)(-s* tm ➜))

  (def-function-class -s (tm &optional ➜))
  (defun-typed -s ((tm tape-machine-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜leftmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜leftmost]
      ))
  (defun-typed -s ((tm tape-machine-parked) &optional ➜)(s* tm ➜))


  (def-function-class sn (tm n &optional ➜))
  (defun-typed sn ((tm tape-machine-empty) n &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= 0 n)
        [➜ok]
        [➜rightmost]
        )))
  (defun-typed sn ((tm tape-machine-parked) n &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (cond
        ((< 0 n) (sn tm (- n) ➜))
        ((= 0 n) [➜ok])
        (t
          (s tm
            {
              :➜ok (λ()(sn tm (1- n) ➜))
              :➜rightmost ➜rightmost
              }))
        )))


  (def-function-class -sn (tm n &optional ➜))
  (defun-typed -sn ((tm tape-machine-empty) n &optional ➜)
    (declare (ignore tm n))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= 0 n)
        [➜ok]
        [➜rightmost]
        )))
  (defun-typed -sn ((tm tape-machine-parked) n &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜leftmost (be ∅))
        &allow-other-keys
        )
      ➜
      (cond
        ((< 0 n) (sn tm (- n) ➜))
        ((= 0 n) [➜ok])
        (t
          (s tm
            {
              :➜ok (λ()(sn tm (1- n) ➜))
              :➜leftmost ➜leftmost
              }))
        )))

  ;; '*' is zero or more times
  (def-function-class s* (tm &optional ➜)) ; move to rightmost
  (defun-typed s* ((tm tape-machine-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))

  (def-function-class -s* (tm &optional ➜)) ; move to leftmost
  ;; step backwards zero or more times
  (defun-typed -s* ((tm tape-machine-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))

  ;; s= step then compare tm=
  ;;     ➜rightmost if tm0 can not be stepped
  ;;     ➜∅ if not tm= after step
  ;;     ➜t if tm= after step
  ;;
  ;; by contract, tm0 and tm1 are entangled
  ;; because they are entangled they are of the same type (thus far in this implementation)
  ;; tm0 empty implies that tm1 is empty, because they are entangled
  ;; tm0 parked, tm1 can be active, vice versa
  ;;
    (defun-typed s= (tm0 tm1 &optional ➜))
    (defun-typed s= 
      (
        (tm0 tape-machine-empty)
        (tm1 tape-machine)
        &optional ➜
        )
      (destructuring-bind
        (
          &key
          (➜rightmost0 (be ∅))
          &allow-other-keys
          )
        ➜
        [➜rightmost0]
        ))
    (defun-typed s= 
      (
        (tm0 tape-machine-parked-or-active)
        (tm1 tape-machine-empty-or-parked)
        &optional ➜
        )
      (s tm0 ➜)
      )
    (defun-typed s= 
      (
        (tm0 tape-machine-active)
        (tm1 tape-machine-active)
        &optional ➜
        )
      (destructuring-bind
        (
          &key
          (➜rightmost (be ∅))
          &allow-other-keys
          )
        ➜
        (s tm0
          {
            :➜ok (λ()(tm= tm0 tm1 ➜))
            :➜rightmost ➜rightmost
            })
        ))

  ;; ses= step, make entangled copy, step again, tm=
  ;;
  ;;     ➜rightmost0 if tm0 can not be stepped
  ;;     ➜rightmost1 if the entangled version of tm0 can not be stepped
  ;;     ➜∅ if not tm= after steps
  ;;     ➜t if tm= after steps
  ;;
  ;; by contract, tm0 and tm1 are entangled
  ;; because they are entangled they are of the same type (thus far in this implementation)
  ;; tm0 empty implies that tm1 is empty, because they are entangled
  ;; tm0 parked, tm1 can be active, vice 
  ;;
  ;; need to finish this ...
  ;;
    (def-function-class ses= (tm0 tm1 &optional ➜))
    (defun-typed ses=
      (
        (tm0 tape-machine-empty)
        (tm1 tape-machine)
        &optional ➜
        )
      (declare (ignore tm0 tm1))
      (destructuring-bind
        (
          &key
          (➜rightmost0 (be ∅))
          &allow-other-keys
          )
        ➜
        [➜rightmost0]
        ))
    (defun-typed ses= 
      (
        (tm0 tape-machine-parked-or-active)
        (tm1 tape-machine-empty-or-parked)
        &optional ➜
        )
      (s tm0 ➜)
      )
    (defun-typed ses= 
      (
        (tm0 tape-machine-parked-or-active)
        (tm1 tape-machine-active)
        &optional ➜
        )
      (destructuring-bind
        (
          &key
          (➜rightmost0 (be ∅))
          (➜rightmost1 (be ∅))
          &allow-other-keys
          )
        ➜
        (s tm0
          {
            :➜ok (λ()
                   (with-entangled tm0
                     (λ(etm0)
                       (s etm0
                         {
                           :➜ok (λ()(tm= etm0 tm1 ➜))
                           :➜rightmost ➜rightmost1
                           }))))

            :➜rightmost ➜rightmost0
            })
        ))

  ;; cues the head to parked
  (def-function-class p (tm &optional ➜)) 
  ;; an empty machine is already parked
  (defun-typed p ((tm tape-machine-empty-or-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))
  (defun-typed p ((tm tape-machine-active) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-parked tm)
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;; topology modification
;;
  (def-function-class epa (tm instance &optional ➜)
    (:documentation
      "Allocates a cell to the left of leftmost (thus becoming the new leftmost).
      "
      ))
  (def-function-class ◨a (tm instance &optional ➜)
    (:documentation
      "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
      ))
  ;; #'◨a  to rightmost of an empty machine is same as #'epa
  ;; this works because parked position acts like rightmost
  (defun-typed ◨a ((tm tape-machine-empty) instance &optional ➜)
    (epa tm instance ➜)
    )

  (def-function-class a (tm instance &optional ➜)
    (:documentation
      "If no cells are available, ➜no-alloc.  Otherwise, allocate a new cell and place
         it to the right of the cell the head is currently on.  The newly allocated cell will
         be initialized with the given instance.
         "))
  ;; #'a to an empty machine is the same as #'epa. Specialized types may depend on this
  ;; synonym being present, and thus not implement their own #'a.
  (defun-typed a ((tm tape-machine-empty-or-parked) instance &optional ➜)
    (epa tm instance ➜)
    )

  (def-function-class -a (tm instance &optional ➜)
    (:documentation
      "If no cells are available, ➜no-alloc.  Otherwise, allocate a new cell and place
       it to the left of the cell the head is currently on.  The newly allocated cell will
       be initialized with the given instance. This function is not available for
       singly linkedin lists.
       "))
  ;; interesting asymetry with #'a
  (defun-typed -a ((tm tape-empty) instance &optional ➜)
    (epa tm instance ➜)
    )
  (defun-typed -a ((tm tape-parked) instance &optional ➜)
    (◨a tm instance ➜)
    )

  (def-function-class as (tm instance &optional ➜)
    (:documentation
      "Like #'a, but tm is stepped to the new cell
      "))
  (defun-typed as ((tm tape-machine) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (a tm instance
        {
          :➜ok (λ()(s tm ➜))
          :➜no-alloc ➜no-alloc
          })
      ))

  (def-function-class a&h◨ (tm instance &optional ➜)
    (:documentation
      "#'a with a contract that the head is on rightmost.
      "))
  ;; surely specializations will make better use of the contract
  (defun-typed a&hs* 
    (
      (tm tape-machine)
      instance
      &optional ➜
      )
      (a tm instance ➜)
      )

  (def-function-class as&h◨ (tm instance &optional ➜)
    (:documentation
      "#'as with a contract that the head is on rightmost.
      "))
   ;; surely specializations will make better use of the contract
  (defun-typed as&hs* 
    (
      (tm tape-machine)
      instance
      &optional ➜
      )
    (as tm instance ➜)
    )


  ;; Spill can be ∅, in which case we just drop the deallocated cell.  When spill is not ∅,
  ;; then the deallocated cell is moved to spill, or a new allocation is made on spill and
  ;; the instance from the deallocated cell is moved to it, preferably the former. 
  ;;
  ;; d must have transactional behavior, i.e. the cell is only dealloced if all goes well,
  ;; otherwise d makes no structural changes.  E.g. d will fail if spill is not nil, and
  ;; reallocation to spill fails
  ;;
    (def-function-class epd (tm &optional spill ➜)
      (:documentation
        "Deallocates leftmost.
         Returns the instance from the deallocated cell.
         If spill is not ∅, the deallocated cell is moved to spill, or a new
         cell is allocated to spill and the instance reference is moved there.
        "
        ))
    (defun-typed epd ((tm tape-machine-empty) &optional spill ➜)
      (declare (ignore spill))
      (destructuring-bind
        (
          &key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        ;; (prins (print "epd empty"))
        [➜empty]
        ))

    ;; delete the whole tape
    (def-function-class epd*(tm &optional spill ➜))
    (defun-typed epd* ((tm tape-machine-empty) &optional spill ➜)
      (declare (ignore spill))
      (destructuring-bind
        (
          &key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        [➜ok]
        ))
    (defun-typed epd* ((tm tape-machine) &optional (spill tape-machine) ➜)
      (destructuring-bind
        (
          &key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (labels(
                 (work ()
                   (epd tm spill
                     {
                       :➜ok #'work
                       :➜rightmost ➜ok
                       }
                     ))
                 )
          (work)
          )))

    (def-function-class d (tm &optional spill ➜)
      (:documentation
        "Deallocate the right neighbor of the cell the head is on.
         I.e. deallocates a region of length 1 located to the right of the head.
         Returns the instance from the deallocated cell.
         If spill is not ∅, the deallocated cell is moved to spill, or a new
         cell is allocated to spill and the instance reference is moved there.
        "
        ))
    (defun-typed d ((tm tape-machine-empty) &optional spill ➜)
      (declare (ignore tm spill))
      (destructuring-bind
        (
          &key
          (➜empty #'accessed-empty)
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))
    (defun-typed d ((tm tape-machine-parked) &optional spill ➜)
      (epd tm spill ➜)
      )

    (def-function-class d. (tm &optional spill ➜))
    (defun-typed d. ((tm tape-machine-empty) &optional spill ➜)
      (declare (ignore tm spill))
      (destructuring-bind
        (&key
          (➜empty (λ()(error 'accessed-empty)))
          &allow-other-keys
          )
        ➜
        [➜empty]
        ))

    ;; delete the right hand side
    (def-function-class d*(tm &optional spill ➜))
    (defun-typed d* ((tm tape-machine-empty) &optional spill ➜)
      (declare (ignore spill))
      (destructuring-bind
        (
          &key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        [➜ok]
        ))
    (defun-typed d* ((tm tape-machine) &optional (spill tape-machine) ➜)
      (destructuring-bind
        (
          &key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (labels(
                 (work ()
                   (d tm spill
                     {
                       :➜ok #'work
                       :➜rightmost ➜ok
                       }
                     ))
                 )
          (work)
          )))


  ;; this function is private. intended to be used with entanglement accounting.
  ;; after another machine in the entanglement group does an epa, we need to
  ;; update the tape reference for the other memebers of the group.
  (def-function-class update-tape-after-epa (tm tm-ref))

  ;; this function is private. intended to be used with entanglement accounting.
  ;; after another machine in the entanglement group does an epd, we need to
  ;; update the tape reference for the other memebers of the group.
  (def-function-class update-tape-after-epd (tm tm-ref))




