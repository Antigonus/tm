#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
|#
(in-package #:tm)

(def-function-class singleton (tm &optional cont-true cont-false))
(def-function-class doubleton (tm &optional cont-true cont-false))
(def-function-class tripleton (tm &optional cont-true cont-false))

(defun-typed singleton (tm &optional (cont-true (be t)) (cont-false (be ∅)))
  (with-mk-entangled tm
    (λ(tm1)
      (cue-leftmost tm1)
      (on-rightmost tm1
        cont-true
        cont-false
        ))))
  
(defun-typed doubleton (tm &optional (cont-true (be t)) (cont-false (be ∅)))
  (with-mk-entangled tm
    (λ(tm1)
      (cue-leftmost tm1)
      (if (∧
            (s tm1)
            (on-rightmost tm1)
            )
        (funcall cont-true)
        (funcall cont-false)
        ))))

(defun-typed tripleton (tm &optional (cont-true (be t)) (cont-false (be ∅)))
  (with-mk-entangled tm
    (λ(tm1)
      (cue-leftmost tm1)
      (if
        (∧
          (s tm1)
          (s tm1)
          (on-rightmost tm1)
          )
        (funcall cont-true)
        (funcall cont-false)
        ))))

;; for more than trippleton, use #'cue-leftmost #'length=


;;--------------------------------------------------------------------------------
;; cmp
;;
  ;; (length tm0) <?> ( (length tm1) | n )
  (def-function-class length-cmp (tm0 n-or-tm1 &optional cont-longer cont-same cont-shorter))

  ;; (length tm0) <?> n
  (defun-typed length-cmp
    (
      (tm0 nd-tape-machine)
      (n integer)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (with-mk-entangled tm0
      (λ(tm0-1)
        (⟳ (λ(cont-loop cont-return) 
             (when (≤ n 0) (return-from length-cmp (funcall cont-longer)))
             (decf n)
             (s tm0-1 cont-loop cont-return)
             ))
        (if 
          (= n 0) 
          (funcall cont-same)
          (funcall cont-shorter)
          ))))

    ;; (length tm0) <?> (length tm1)
  (defun-typed length-cmp
    (
      (tma nd-tape-machine)
      (tmb nd-tape-machine)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )

    (with-mk-entangled tma
      (λ(tma-1)
        (with-mk-entangled tmb
          (λ(tmb-1)
            (let(
                  (tms (mk 'tape-machine :mount {tma-1 tmb-1}))
                  )
              (⟳-loop
                (λ(cont-loop)
                  (let(
                        (on-rm-a (on-rightmost tma-1))
                        (on-rm-b (on-rightmost tmb-1))
                        )
                    (cond
                      ((∧ on-rm-a on-rm-b)
                        (return-from length-cmp (funcall cont-same)))
                      (on-rm-b
                        (return-from length-cmp (funcall cont-longer)))
                      (on-rm-a
                        (return-from length-cmp (funcall cont-shorter))
                        )
                      ))
                  (s-together tms cont-loop #'cant-happen)
                  ))))))))

;;--------------------------------------------------------------------------------
;; all possible reduction to a Boolean operator, excluding constants true and false.
;;

  ;;000 false

  ;;001
  (defun length< 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-false cont-true)
    )

  ;;010
  (defun length= 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-true cont-false)
    )

  ;;011
  (defun length≤ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-false cont-true cont-true)
    )

  ;;100
  (defun length> 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-false cont-false)
    )

  ;;101
  (defun length≠ 
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-false cont-true)
    )

  ;;110
  (defun length≥
   (
     tm0 
     n-or-tm1
     &optional
     (cont-true (be t))
     (cont-false (be ∅))
     )
    (length-cmp tm0 n-or-tm1 cont-true cont-true cont-false)
    )

   ;;111 true

