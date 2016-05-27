#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglement scope
;;
;; (with-copies ((cp0 tm0) (cp1 tm1) ..) body-form ..)
;;
;; expands to:
;; 
;; (let(cp0 cp1 ..)
;;    (unwind-protect
;;      (progn
;;         (setq cp0 (copy mach0))
;;         (setq cp1 (copy mach1))
;;         ..
;;         body
;;      )
;;      (when cp0 (disentangle cp0))
;;      (when cp1 (disentangle cp1))
;;          ..
;;    )
;;
;;  this expansion works, but it would be higher performance to not lose the
;;  the copy references up top, then to just deallocate them directly
;;
  (defun make-copy-decl (defs)
    (let(
          (tm-defs (mount defs))
          (copy-decl (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (copy-name (car (r tm-defs)))
                )
            (as copy-decl copy-name)
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount copy-decl)
      ))

  (defun make-copy-prog (defs)
    (let(
          (tm-defs (mount defs))
          (copy-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (copy-name (car (r tm-defs)))
                (mach-name (cadr (r tm-defs)))
                )
            (as copy-prog `(setq ,copy-name (copy ,mach-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount copy-prog)
      ))

  ;; makes the disentangle program
  (defun make-dis-prog (defs)
    (let(
          (tm-defs (mount defs))
          (dis-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (copy-name (car (r tm-defs)))
                )
            (as dis-prog `(when ,copy-name (disentangle ,copy-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount dis-prog)
      ))

  (defmacro with-copies (defs &body body)
    (unless defs (return-from with-copies ∅))
    (let(
          (cp-decl (make-copy-decl defs))
          (cp-prog (make-copy-prog defs))
          (dis-prog (make-dis-prog defs))
          )
      (let(
            (prog 
              `(let ,cp-decl
                 (unwind-protect
                   (progn
                     ,@cp-prog
                     ,@body
                     )
                   ,@dis-prog
                   )))
            )
        ;; (pprint prog)
        prog
        )))

