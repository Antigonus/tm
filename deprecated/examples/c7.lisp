
;;; I used this as a copy paste scratch pad rather than as a load file


(defun c7-0 (x) (setf x (cons 7 x)))
(defun c7-1 (x) (cons 7 x))
(defun c7-2 (x) (append x {7}))
(defun c7-3 (x) (nconc x {7}))

(let(
      (a0-0 ∅)
      (a0-1 ∅)
      (a0-2 ∅)
      (a0-3 ∅)
      (a1-0 {'a})
      (a1-1 {'a})
      (a1-2 {'a})
      (a1-3 {'a})
      )

  (c7-0 a0-0)
  (c7-1 a0-1)
  (c7-2 a0-2)
  (c7-3 a0-3)

  (c7-0 a1-0)
  (c7-1 a1-1)
  (c7-2 a1-2)
  (c7-3 a1-3)

  (print "a0-0:")(princ a0-0)
  (print "a0-1:")(princ a0-1)
  (print "a0-2:")(princ a0-2)
  (print "a0-3:")(princ a0-3)
                     
  (print "a1-0:")(princ a1-0)
  (print "a1-1:")(princ a1-1)
  (print "a1-2:")(princ a1-2)
  (print "a1-3:")(princ a1-3)
  )


(defmacro c7 (x) `(setf ,x (cons 7 ,x)))

(let(
      (a0 ∅)
      (a1 {'a})
      )
  (c7 a0)
  (c7 a1)
  (print "a0:")(princ a0)
  (print "a1:")(princ a1)
  )


(defun c7 (x) (setf (cdr x) (cons 7 (cdr x)))) 

(let(
      (a0 {'padding})
      (a1 {'padding 'a})
      )
  (c7 a0)
  (c7 a1)
  (print "a0:")(princ (cdr a0))
  (print "a1:")(princ (cdr a1))
  )
