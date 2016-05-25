#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; removing an entanglement
;; 
;;
  ;; This is built specifically around entanglements that use tm-list so as to avoid
  ;; circular creation of entanglements. None of the functions used here create
  ;; entanglements.
  (defun disentangle (tm)
    "Removes tm from the associated entanglements set. This is done, for example, 
     in the cue-to function to release the instance for reuse.
     "
    (let(
          (es (entanglements tm)) ; es must be type tm-list
          )
      (unless es (return-from disentangle))
      (let(
            (e (r◧-tm-list es))
            )
        (when 
          (eq e tm)
          (d◧-tm-list es)
          (return-from disentangle)
          ))
      (cue-leftmost es)
      (⟳(λ(cont-loop cont-return)
          (r-index-tm-list es 1
            (λ(e) 
              (when (eq e tm) 
                (d es)
                (return-from disentangle) ; found it, so return
                ))
            (λ() ; typical return, index fails because we are on rightmost
              (return-from disentangle)
              ))
          (s es cont-loop cont-return) ; cont-return case for e not found
          ))
      ))

  (defun test-disentangle-0 ()
    (let(
          (a (mount {1 2 3}))
          )
      (disentangle a)
      (empty (entanglements a))
      ))
  (test-hook test-disentagle-0)
             
  (defun ds-∃-collision (tm0 &optional (cont-true (be t)) (cont-false (be ∅)))
    "dup-1 tm0, step tm0, ask if tm0 has any collisions with entangled machines."
    (on-rightmost tm0
      cont-false
      (λ()
        (let(
              (tm1 (dup tm0))
              )
          (s tm1 #'do-nothing #'cant-happen)
          (∃-collision tm1 cont-true cont-false)
          ))))

;;--------------------------------------------------------------------------------
;; entanglement scope
;;
;; (with-dups ((dup0 tm0) (dup1 tm1) ..) body-form ..)
;;
;; expands to:
;; 
;; (let(dup0 dup1 ..)
;;    (unwind-protect
;;      (progn
;;         (setq dup0 (dup mach0))
;;         (setq dup1 (dup mach1))
;;         ..
;;         body
;;      )
;;      (when dup0 (disentangle dup0))
;;      (when dup1 (disentangle dup1))
;;          ..
;;    )
;;
;;  this expansion works, but it would be higher performance to not lose the
;;  the dup references up top, then to just deallocate them directly
;;
  (defun make-dup-decl (defs)
    (let(
          (tm-defs (mount defs))
          (dup-decl (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (dup-name (car (r tm-defs)))
                )
            (as dup-decl dup-name)
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount dup-decl)
      ))

  (defun make-dup-prog (defs)
    (let(
          (tm-defs (mount defs))
          (dup-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (dup-name (car (r tm-defs)))
                (mach-name (cadr (r tm-defs)))
                )
            (as dup-prog `(setq ,dup-name (dup ,mach-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount dup-prog)
      ))

  (defun make-dis-prog (defs)
    (let(
          (tm-defs (mount defs))
          (dis-prog (mk 'tm-list))
          )
      (⟳(λ(cont-loop cont-return)
          (let(
                (dup-name (car (r tm-defs)))
                )
            (as dis-prog `(when ,dup-name (disentangle ,dup-name)))
            )
          (s tm-defs cont-loop cont-return)
          ))
      (unmount dis-prog)
      ))

  (defmacro with-dups (defs &body body)
    (unless defs (return-from with-dups ∅))
    (let(
          (dup-decl (make-dup-decl defs))
          (dup-prog (make-dup-prog defs))
          (dis-prog (make-dis-prog defs))
          )
      (let(
            (prog 
              `(let ,dup-decl
                 (unwind-protect
                   (progn
                     ,@dup-prog
                     ,@body
                     )
                   ,@dis-prog
                   )))
            )
        ;; (pprint prog)
        prog
        )))

