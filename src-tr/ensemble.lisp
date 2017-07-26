#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Base is a tape machine, where the instances found on the tape are tape machines.  These
leaf tape machines are members of the ensemble.

Operations on the ensemble affect all members of the ensemble.  For example,
stepping the ensemble steps all the member machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (def-type ensemble-tr (tape-machine)
    (
      (members ; members is a machine, where each instance on the tape is a machine
        :initarg :members
        :accessor members
        )
      ))

;;--------------------------------------------------------------------------------
;; making transform machines
;;
  (defun-typed init 
    (
      (tm ensemble-tr)
      &optional
      init-value
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        )
      ➜
      (destructuring-bind
        (&key list machine) init-value
        (cond
          ((∧ list (consp list))
            (setf (members tm) (mk 'list-tm {:tape list}))
            [➜ok tm]
            )
          ((∧ machine (typep machine 'tape-machine))
            (setf (members tm) machine)
            [➜ok tm]
            )
          (t [➜fail])
          ))))

   ;; we do not entangle ensembles .. perhaps in the future

;;--------------------------------------------------------------------------------
;;tm-decl-only
;;
  ;;--------------------------------------------------------------------------------
  ;; accessing data
  ;;

    ;; sometimes better to just read the member machines directly
    ;; returns a machine of read values
    ;; the return machine has the same type as the first member
    (defun-typed r ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (let(
                (result (mk (type-of (r mtm)))) ; result type same as that of first member
                )
            (∀* mtm (λ(mtm)
                      (as result (r (r mtm))
                        {
                          :➜no-alloc (λ()(return-from r [➜no-alloc]))
                          })))
            [➜ok result]
            ))))

    (defun-typed esr ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜bound-right (λ()(error 'step-from-bound-right)))
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (let(
                (result (mk (type-of (r mtm)))) ; result type same as that of first member
                )
            (∀ mtm
              (λ(mtm ct c∅)
                (esr (r mtm)
                  {:➜ok (λ(instance)
                          (as result instance
                            {
                              :➜ok ct
                              :➜no-alloc (λ()(return-from esr [➜no-alloc]))
                              }
                            ))
                    :➜bound-right c∅
                    }))
              {
                :➜t (λ()[➜ok result])
                :➜∅ ➜bound-right
                }
              )))))

    (defun-typed w ((tm ensemble-tr) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (-s*∀* (members tm) (λ(mtm)(w (r mtm) instance)))
        [➜ok]
        ))

    (defun-typed esw ((tm ensemble-tr) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          (➜bound-right (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (∀ mtm
            (λ(mtm ct c∅)
              (esw (r mtm) {:➜ok ct :➜bound-right c∅})
              )
            {
              :➜t ➜ok
              :➜∅ ➜bound-right
              }
            ))))

    (defun-typed ◧r ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (let(
                (result (mk (type-of (r mtm)))) ; result type same as that of first member
                )
            (∀* mtm (λ(mtm)
                      (as result (◧r (r mtm))
                        {
                          :➜no-alloc (λ()(return-from ◧r [➜no-alloc]))
                          })))
            [➜ok result]
            ))))

    (defun-typed ◧sr ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜bound-right (λ()(error 'step-from-bound-right)))
          (➜no-alloc #'alloc-fail)
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (let(
                (result (mk (type-of (r mtm)))) ; result type same as that of first member
                )
            (∀ mtm
              (λ(mtm ct c∅)
                (◧sr (r mtm)
                  {:➜ok (λ(instance)
                          (as result instance
                            {
                              :➜ok ct
                              :➜no-alloc (λ()(return-from ◧sr [➜no-alloc]))
                              }
                            ))
                    :➜bound-right c∅
                    }))
              {
                :➜t (λ()[➜ok result])
                :➜∅ ➜bound-right
                }
              )))))

    (defun-typed ◧w ((tm ensemble-tr) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (-s*∀* (members tm) (λ(mtm)(◧w (r mtm) instance)))
        [➜ok]
        ))

  (defun-typed ◧sw ((tm ensemble-tr) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜bound-right (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (mtm (members tm))
            )
        (-s* mtm)
        (∀ mtm
          (λ(mtm ct c∅)
            (◧sw (r mtm) {:➜ok ct :➜bound-right c∅})
            )
          {
            :➜t ➜ok
            :➜∅ ➜bound-right
            }
          ))))


  ;;--------------------------------------------------------------------------------
  ;; absolute head placement
  ;;
    ;; if a member is an empty status machine ?
    (defun-typed -s* ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (-s*∀* (members tm) (λ(mtm)(-s* (r mtm))))
        [➜ok]
        ))

  ;;--------------------------------------------------------------------------------
  ;; head stepping
  ;;
    ;; if there exists a member machine on bound-right, the ensemble is on bound-right
    (defun-typed s ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          (➜bound-right (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s*∃ mtm (λ(mtm ct c∅)(on-bound-right (r mtm) {:➜t ct :➜∅ c∅}))
            {
              :➜t ➜bound-right
              :➜∅ (λ()(-s*∀*
                        mtm
                        (λ(mtm)
                          (s (r mtm) {:➜ok #'do-nothing :➜bound-right #'cant-happen})
                          ))
                    [➜ok]
                    )
              }))))

  ;;--------------------------------------------------------------------------------
  ;; cell allocation
  ;;
    ;; no 'a' support

  ;;--------------------------------------------------------------------------------
  ;; location
  ;;  
    ;; any on bound-left - predicts if -s would take a bound-left continuation
    (defun-typed on-bound-left ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (∃ mtm
            (λ(mtm ct c∅)
              (on-bound-left (r mtm) {:➜t ct :➜∅ c∅})
              )
            {
              :➜t ➜t
              :➜∅ ➜∅
              }
            ))))

    ;; any on bound-right - predicts if step would take a bound-right continuation
    (defun-typed on-bound-right ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (∃ mtm
            (λ(mtm ct c∅)
              (on-bound-right (r mtm) {:➜t ct :➜∅ c∅})
              )
            {
              :➜t ➜t
              :➜∅ ➜∅
              }
            ))))

  ;;--------------------------------------------------------------------------------
  ;; length-tape
  ;;
    ;; would ◧s take a bound-right continuation?
    (defun-typed tape-length-is-one ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (∃ mtm
            (λ(mtm ct c∅)
              (tape-length-is-one (r mtm) {:➜t ct :➜∅ c∅})
              )
            {
              :➜t ➜t
              :➜∅ ➜∅
              }
            ))))

    ;; would ◧ss take a bound-right continuation?
    (defun-typed tape-length-is-two ((tm ensemble-tr) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        (let(
              (mtm (members tm))
              )
          (-s* mtm)
          (∃ mtm
            (λ(mtm ct c∅)
              (tape-length-is-two (r mtm) {:➜t ct :➜∅ c∅})
              )
            {
              :➜t ➜t
              :➜∅ ➜∅
              }
            ))))

