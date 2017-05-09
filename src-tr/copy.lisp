#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Copying Machines

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-function-class copy-shallow (src dst &optional ➜))
  (def-function-class copy-shallow-fit (src dst))

  ;; src is a resource we pull from
  ;; dst is a container we are filling
  (defun-typed copy-shallow ((src tape-machine) (dst tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜src-depleted (be ∅)) ;; but still room on dst
        (➜dst-full (be ∅))  ;; but still instances uncopied from src
        &allow-other-keys
        )
      ➜
      (let(
            (ens (mk 'ensemble-tr {:list {src dst}}))
            )
        (∀* ens (λ(ens)(declare (ignore ens)) (w dst (r src))))
        (cond
          ((∧ (on-rightmost src) (on-rightmost dst)) [➜ok])
          ((on-rightmost src) [➜src-depleted])
          (t [➜dst-full])
          ))))

  ;; the Procrustean version ..
  (defun-typed copy-shallow-fit ((src tape-machine) (dst tape-machine))
    (copy-shallow src dst
      {
        :➜ok (be t)

        :➜src-depleted
        (λ()(d* dst))

        :➜dst-full
        (λ()
          (s src {:➜ok #'do-nothing :➜rightmost #'cant-happen})
          (∀* src (λ(src)(as dst (r src)))))
        }))

#|
  (defun test-copy-shallow ()
    (let(
          (tm0 (mk 'list-tm {:tape {1 2 3 4}}))
          (tm1 (mk 'list-tm {:
|#        
