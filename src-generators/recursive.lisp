#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'recursive' repeatedly applies a function to create the successive elements of
a sequence.

An example is a the set of natural numbers:  
(defun f (i c-ok c-fail)(declare (ignore c-fail))[c-ok (+ i 1)]):

  0 1 2 3 ... 
  0 f(0) f(f(0)) f(f(f(0)))

We allow that there is a rightmost instance in 'recursive', so this may be 
a sequence:

  a b c .. z

The Fibernocci series is not an example, as one needs knowledge of the prior two 
values, rather than just the last one.  Though we can make a number pair the single
value, and work with that.  Though, then reading the machine would return the 
pair instead of the correct value.

The 'recursive' type consists of an 'initial' instance, which is provided with the mk call,
an 'head' instance which is calculated, and a 'f'unction which is to be repeatedly
applied.

The function accepts an instance and then has two continuations. When the first
continuation is called, it is called with a computed value.  When the second continuation
is called the function did not compute a value.  Perhaps because there is no next
value in the sequence.

The initial value defaults to 0.  The function must be provided. 

In this implementation we don't count the number of recursive applications (i.e. the
address of the cell), and analyzing the properties of 'f' is beyond our scope, so we don't
implement the comparison of two machines as per #'head-on-same-cell.  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (def-type recursive (tape-machine)
    (
      (initial
        :initarg initial
        :accessor initial
        )

      (f
        :initarg f
        :accessor f
        )

      (head
        :initarg head
        :accessor head
        )

      ))

;;--------------------------------------------------------------------------------
;; making transform machines
;;
  (defun-typed init 
    (
      (tm recursive)
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
        (&key initial f) init-value
        (if initial
          (setf (initial tm) initial)
          (setf (initial tm) 0)
          )
        (setf (head tm) (initial tm))
        (if f
          (progn 
            (setf (f tm) f)
            [➜ok tm]
            )
          [➜fail]
          )
        )))

   ;; recursive can not be entangled


;;--------------------------------------------------------------------------------
;;tm-decl-only
;;
  ;;--------------------------------------------------------------------------------
  ;; accessing data
  ;;
    (defun-typed r ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys
          )
        ➜
        [➜ok (head tm)]
        ))

    (defun-typed esr ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜rightmost (λ()(error 'step-from-rightmost)))
          &allow-other-keys
          )
        ➜
        [(f tm)
          (head tm)
          ➜ok
          ➜rightmost
          ]
        ))

    (defun-typed w ((tm recursive) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (setf (head tm) instance)
        [➜ok]
        ))

    ;; recursive has no esw function

    (defun-typed ec◧r ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          )
        ➜
        [➜ok (initial tm)]
        ))

    (defun-typed ec◧sr ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo)
          (➜rightmost (λ()(error 'step-from-rightmost)))
          &allow-other-keys
          )
        ➜
        [(f tm) (initial tm)
          ➜ok
          ➜rightmost
          ]
        ))

    (defun-typed ec◧w ((tm recursive) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (setf (initial tm) instance)
        [➜ok]
        ))

    ;; recursive has no ec◧sw function

  ;;--------------------------------------------------------------------------------
  ;; absolute head placement
  ;;
    (defun-typed c◧ ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (setf (head tm) (initial tm))
        [➜ok]
        ))


  ;;--------------------------------------------------------------------------------
  ;; head stepping
  ;;
    (defun-typed s ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          (➜rightmost (be ∅))
          &allow-other-keys
          )
        ➜
        [(f tm) (head tm)
          (λ(ip1)
            (setf (head tm) ip1)
            [➜ok]
            )
          ➜rightmost
          ]
        ))

  ;;--------------------------------------------------------------------------------
  ;; cell allocation
  ;;
    ;; no 'a' support

  ;;--------------------------------------------------------------------------------
  ;; location
  ;;  
    (defun-typed on-leftmost ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        (if (eq (initial tm) (head tm)) [➜t] [➜∅])
        ))

    (defun-typed on-rightmost ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        [(f tm) (head tm)
           (λ(ip1)(declare (ignore ip1))[➜∅])
           ➜t
           ]
        ))

  ;;--------------------------------------------------------------------------------
  ;; length-tape
  ;;
    (defun-typed tape-length-is-one ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        [(f tm) (initial tm)
          (λ(ip1)(declare (ignore ip1)) [➜∅])
          ➜t
          ]))

    (defun-typed tape-length-is-two ((tm recursive) &optional ➜)
      (destructuring-bind
        (&key
          (➜t (be t))
          (➜∅ (be ∅))
          &allow-other-keys
          )
        ➜
        [(f tm) (initial tm)
          (λ(ip1)
            [(f tm) ip1
              (λ(ip1)(declare (ignore ip1))[➜∅])
              ➜t
              ])
          ➜∅
          ]))

;;--------------------------------------------------------------------------------
;; utilities
;;
  (defun increment-to (b &optional (stride 1))
    (λ(i0 c-ok c-fail)
      (let(
            (i1 (+ i0 stride))
            )
        (if (≤ i1 b) [c-ok i1] [c-fail])
        )))

  (defun decrement-to (a &optional (stride 1))
    (λ(i0 c-ok c-fail)
      (let(
            (i1 (- i0 stride))
            )
        (if (≥ i1 a) [c-ok i1] [c-fail])
        )))

  (defun mk-interval (a b &optional (stride 1))
    (mk 'recursive {:initial a :f (increment-to b stride)})
    )

  ;; of course Lisp is not case sensitive, this usage of case is a convention
  (defun mk-Natural ()
    (labels(
             (sucessor (i0 ct c∅)
               (declare (ignore c∅))
               (let(
                     (i1 (+ i0 1))
                     )
                 [ct i1]
                 ))
             )
      (mk 'recursive {:initial 0 :f #'sucessor})
      ))
