#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglement scope
;;
;; (with-forks ((fk0 tm0) (fk1 tm1) ..) body-form ..)
;;
;; expands to:
;; 
;; (let(fk0 fk1 ..)
;;    (unwind-protect
;;      (progn
;;         (setq fk0 (fork mach0))
;;         (setq fk1 (fork mach1))
;;         ..
;;         body
;;      )
;;      (when fk0 (disentangle fk0))
;;      (when fk1 (disentangle fk1))
;;          ..
;;    )
;;
;;  this expansion works, but it would be higher performance to not lose the
;;  the fork references up top, then to just deallocate them directly
;;
  (defun make-fork-decl (defs)
    (let(
          (tm-defs (mount defs))
          (fork-decl (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (fork-name (car (r tm-defs)))
                )
            (as fork-decl fork-name)
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount fork-decl)
      ))

  (defun make-fork-prog (defs)
    (let(
          (tm-defs (mount defs))
          (fork-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (fork-name (car (r tm-defs)))
                (mach-name (cadr (r tm-defs)))
                )
            (as fork-prog `(setq ,fork-name (fork ,mach-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount fork-prog)
      ))

  ;; makes the disentangle program
  (defun make-dis-prog (defs)
    (let(
          (tm-defs (mount defs))
          (dis-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (fork-name (car (r tm-defs)))
                )
            (as dis-prog `(when ,fork-name (disentangle ,fork-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount dis-prog)
      ))

  (defmacro with-forks (defs &body body)
    (unless defs (return-from with-forks ∅))
    (let(
          (fk-decl (make-fork-decl defs))
          (fk-prog (make-fork-prog defs))
          (dis-prog (make-dis-prog defs))
          )
      (let(
            (prog 
              `(let ,fk-decl
                 (unwind-protect
                   (progn
                     ,@fk-prog
                     ,@body
                     )
                   ,@dis-prog
                   )))
            )
        ;; (pprint prog)
        prog
        )))

