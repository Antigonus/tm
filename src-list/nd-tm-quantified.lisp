#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification 

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (def-function-class esnr (tm index &optional ➜)
    (:documentation
      " This is an indexed read operation.  It makes an entangled copy of 'tm', 
        steps it 'index' times, then reads it.
      "
      ))

  (defun-typed esnr 
    (
      (tm nd-tape-machine)
      index
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (tm1 (entangle tm))
            )
          (sn tm1 index
            {
              :➜ok (λ()[➜ok (r tm1)])
              :➜rightmost (λ(n)[➜rightmost n])
              }
            ))))

  (def-function-class esnw (tm index instsance &optional ➜)
    (:documentation
      " This is an indexed write operation.  It makes an entangled copy of 'tm', 
        steps it 'index' places, then writes the 'instance'.
      "
      ))

  (defun-typed esnw 
    (
      (tm nd-tape-machine)
      index
      instance
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (tm1 (entangle tm))
            )
          (sn tm1 index
            {
              :➜ok (λ()(w tm1 instance {:➜ok ➜ok}))
              :➜rightmost (λ(n)[➜rightmost n])
              }))))

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (def-function-class eas* (tm tm-fill &optional ➜)
    (:documentation 
      "calls #'as repeatedly on a mk-entangled copy of tm, filling
       with successive instances from tm-fill.
       tm will not be stepped.  tm-fill will be stepped.
       "
      ))

  (defun-typed eas*
    (
      (tm0 nd-tape-machine) 
      fill
      &optional ➜
      )
    (let(
          (tm1 (entangle tm0))
          )
      (as* tm1 fill ➜)
      ))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (def-function-class an (tm count tm-fill &optional ➜)
    (:documentation 
      "Similar to calling #'a n times on a mk-entangled-with of tm."
      ))

  (defun-typed an
    (
      (tm nd-tape-machine)
      (n integer)
      fill
      &optional ➜
      )
    (let(
          (tm1 (entangle tm))
          )
      (asn tm1 n fill ➜)
      ))



        
          


