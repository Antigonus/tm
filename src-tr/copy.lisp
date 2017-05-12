#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Copying Machines

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-function-class c (src dst &optional ➜))
  (def-function-class c-fit (src dst))

  ;; src is a resource we pull from
  ;; dst is a container we are filling
  ;;
  ;; after call tape is left on:
  ;;   src
  ;;     ➜ok: rightmost
  ;;     ➜src-depleted: the rightmost cell that has been copied, or if appropriate, empty
  ;;     ➜dst-full: the cell we would have copied had dst not been full
  ;;   dst
  ;;     ➜ok: rightmost
  ;;     ➜src-depleted: the last cell already written, or if appropriate, empty
  ;;     ➜dst-full: the last cell already written, or if appropriate, empty
  ;;
  (defun-typed c ((src tape-machine) (dst tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be 'ok))
        (➜src-depleted (be 'src-depleted)) ;; room remains on dst
        (➜dst-full (be 'dst-full))  ;; uncopied instances remain on src
        &allow-other-keys
        )
      ➜
      (∀ dst
        (λ(dst ct c∅)
          (w dst (r src))
          (s src
            {
              :➜ok ct
              :➜rightmost c∅
              }))
        {
          :➜t ➜dst-full
          :➜∅ (λ()
                (on-rightmost dst
                  {
                    :➜t ➜ok
                    :➜∅ ➜src-depleted
                    }))
          })))

  ;; the Procrustean version ..
  ;; upon entry:
  ;;   src on leftost cell to be copied from
  ;;   dst on leftmost cell to be copied to
  ;;
  (defun-typed c-fit ((src tape-machine) (dst tape-machine))
    (c src dst
      {
        :➜ok (be t)

        :➜src-depleted
        (λ()(d* dst))

        :➜dst-full
        (λ()(∀* src (λ(src)(as dst (r src)))))

        }))


