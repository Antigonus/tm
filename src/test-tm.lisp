#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests are also scattered through the code.  When evaluation order matters,
  put the test here.

  All tests have names of the form test-fun-0  where fun is the function
  or logical concept being tested.

  In tests with an and of many clauses, replace the (and ...) with (pprint (list ...))
  to see the results of individual tests.
  
|#
  (in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-accesories
;;
  (defun test-r-index-0 ()
    (let(
          (k (mk-tm 'tm-list (list 6 7 8)))
          )
      (∧
        (= 6 (r-index k 0))
        (= 7 (r-index k 1))
        (= 8 (r-index k 2))
        (= 1 (r-index k 3
               (λ(x)(declare (ignore x)) ∅)
               #'echo
               )))))
  (test-hook test-r-index-0)

;;--------------------------------------------------------------------------------
;; tm-si
;;
  (defun test-si-0 ()
    (let*(
           (y '(1 2 (3 4) 5))
           (ytm (mk-tm 'tm-list y))
           )
      (s ytm)
      (s ytm)
      (si ytm)
      (eql (r ytm) 3)
      ))
  (test-hook test-si-0)     

  (defun test-si-1 ()
    (let*(
           (y0 (mk-tm 'tm-list '(1 2 3)))
           (y1 (mk-tm 'tm-list `(11 12 ,y0 13)))
           )
      (∧
        (= (r y1) 11)
        (s y1)
        (= (r y1) 12)
        (s y1)
        (si y1)
        (= (r y1) 1)
        (s y1)
        (= (r y1) 2)
        (s y1)
        (= (r y1) 3)
        (¬ (s y1))
        )
      ))
  (test-hook test-si-1)     

;;--------------------------------------------------------------------------------
;; tm-quantifiers
;;
  (defun test-∃-0 ()
    (let*(
           (y '(1 2 (3 4) 5))
           (ytm (mk-tm-list y))
           )
      (∃ ytm (λ(tm)(and (typep (r tm) 'cons) (eql 3 (car (r tm))))))
      (equal (r ytm) '(3 4))
      ))
  (test-hook test-∃-0) 

  (defun test-¬∀-0 ()
    (let*(
           (y '(1 3 4 5))
           (ytm (mk-tm-list y))
           )
      (¬∀ ytm (λ(tm)(and (numberp (r tm)) (oddp (r tm)))))
      (= (r ytm) 4)
      ))
  (test-hook test-¬∀-0) 

  (defun test-d*-0 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list a))
           )
      (d* tm1)
      (equal
        (tape tm1)
        '(1)
        )))
  (test-hook test-d*-0)

  (defun test-sn-0 ()
    (let*(
           (y '(1 3 6 5))
           (tmy (mk-tm-list y))
           )
      (and
        (sn tmy 2
          (λ()(= (r tmy) 6))
          (be ∅)
          )
        (sn tmy 27
          (be ∅)
          (λ(cnt)
            (and (= (r tmy) 5) (= cnt 26))
            )))))
  (test-hook test-sn-0)

  (defun test-sn-1 ()
    (let(
          (k0 (mk-tm 'tm-list (list 10 11 12)))
          (k1 (mk-tm 'tm-list (list 13 14 15)))
          )
      (∧
        (= (r k0) 10)
        (sn k0 1)
        (= (r k0) 11)
        (= (sn k0 22 (be ∅) #'echo) 21)

        (= (r k1) 13)
        (sn k1 2)
        (= (r k1) 15)
        (sn k1 1 (be ∅) (be t))
        )))
  (test-hook test-sn-1)


;;--------------------------------------------------------------------------------
;; length.lisp
;;
  (defun test-ton-0 ()
    (let(
          (a (mk-tm-list))
          (b (mk-tm-list '(1)))
          (c (mk-tm-list '(1 2)))
          (d (mk-tm-list '(1 2 3)))
          )
      (∧
        (∧ (singleton a) (eq (r a) 'list))
        (not (doubleton a))
        (singleton b)
        (not (doubleton b))
        (not (singleton c))
        (not (tripleton c))
        (doubleton c)
        (not (singleton d))
        (not (doubleton d))
        (tripleton d)
        )))
  (test-hook test-ton-0)

  (defun test-length≥-0 () 
    (let(
          (tm (mk-tm-list '(a b c)))
          )
    (and
      (length≥ tm 2)
      (length≥ tm 3)
      (not (length≥ tm 4))
      )))
  (test-hook test-length≥-0)

  (defun test-length=-0 () 
    (let(
          (tm (mk-tm-list '(a b c)))
          )
      (and
        (not (length= tm 2))
        (length= tm 3)
        (not (length= tm 4))
        )))
  (test-hook test-length=-0)





#|
  (defun test-s-map-0 ()
    (let*(
           (tm0 (mk-tm-list '(1 2 3)))
           (tm1 (mk-tm-list))
           )
      (labels(
               (inc(input-object cont-forward &rest rest-conts)
                 (declare (ignore rest-conts))
                 (funcall cont-forward (1+ input-object))
                 )

               (dec(input-object cont-forward &rest rest-conts)
                 (declare (ignore rest-conts))
                 (funcall cont-forward (1- input-object))
                 )
               )
        (∧
          (equal (tape tm1) '(list))
          (s-map tm0 tm1 #'inc)
          (equal (tape tm1) '(list 2))
          (s-map tm0 tm1 #'dec)
          (equal (tape tm1) '(list 2 1))
          (¬ (s-map tm0 tm1 #'inc))
          (equal (tape tm1) '(list 2 1 4))
          ))))

  (defun test-s-map-1 ()
      (labels(
               (inc(input-object cont-forward &rest rest-conts)
                 (declare (ignore rest-conts))
                 (funcall cont-forward (1+ input-object))
                 )
               )
        (let*(
               (tm0 (mk-tm-list '(1 2 3)))
               (tm1 (mk-tm-list))
               )
          (⟳ tm0 
            (λ(tm0 cont-ok cont-rightmost)
              (s-map tm0 tm1 #'inc cont-ok cont-rightmost)
              ))
          (print (tape tm1))
          (equal
            (tape tm1) '(list 2 3 4)
            ))))


;;--------------------------------------------------------------------------------
;; from quantifiers
;;
  (defun test-⟳-0 ()
    (let(
          (tm (mk-tm-list '(a b c)))
          (n  3)
          )
      (⟳ tm #'s (λ()(incf n)))
      (= n 6)
      ))
  (test-hook test-⟳-0)


;;--------------------------------------------------------------------------------
;; from list-L

  (defun test-q-0 () (equal (q a b c) '(a b c)))
  (test-hook test-q-0)

  (defun test-q-1 ()
    (equal '(a (b c) d) (q a (b c) d))
    )
  (test-hook test-q-1)

  (defun test-L-0 ()
    (equal
      (L 1 '(2 3) (o '(4 5)))
      '(1 (2 3) 4 5)
      ))
  (test-hook test-L-0)

  (defun mk-7 () 7)

  (defun test-L-1 ()
    (equal (L (o (q a b mk-7)) (mk-7)) '(A B mk-7 7)))
  (test-hook test-L-1)

|#
