#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; unique to ea
;;
  (def-function-class clean-entanglements (tm))

  ;; machines are considered to be entangled with themselves, thus
  ;; we can never have an empty entanglements machine
  (defun-typed clean-entanglements ((tm ea-tm))
    (let(
          (es (entanglements tm))
          )

      (c◧ es)
      (⟳
        (λ(➜again)
          (if
            (on-rightmost es)
            (return-from clean-entanglements t)
            (when (tg:weak-pointer-value (r es))
              (when (on-leftmost es) (s es {:➜rightmost #'cant-happen}))
              (d◧ es)
              [➜again]
              ))))
      (⟳
        (λ(➜again)
          (if
            (on-rightmost es)
            (return-from clean-entanglements t)
            (if
              (tg:weak-pointer-value (esr es))
              (d es)
              (s es {:➜ok ➜again :➜rightmost (λ()(return-from clean-entanglements t))})
              ))))

      [#'cant-happen]
      ))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;


