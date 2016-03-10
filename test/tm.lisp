#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package :tm)



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


|#
