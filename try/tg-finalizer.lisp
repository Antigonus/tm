#| 
 Trivial garbage finalizer.

|#

;;--------------------------------------------------------------------------------
;; this prints "weak-a still defined"
;; .. using sbcl
;;
  (def-type a-type () 
    (
      (x :initform 23 :accessor x)
      ))

  (defun try-finalizer-0 ()
    (let(
          weak-a
          )
      (let(
            (a (make-instance 'a-type))
            )
        (print (x a))
        (finish-output nil)
        (setf weak-a (tg:make-weak-pointer a))
        (print (x (tg:weak-pointer-value weak-a)))
        )
      (print "a now out of scope")

      (print "running garbage collection")
      (finish-output nil)
      (tg:gc :full :verbose)
      (print "after garbage collection")
      (finish-output nil)

      (if (tg:weak-pointer-value weak-a)
        (print "weak-a still defined")
        (print "weak-a no longer defined")
        )
      'done
      ))

;;--------------------------------------------------------------------------------
;; however, this does print "weak-a-1 no longer defined"
;;
  (defvar weak-a-1)

  (defun try-finalizer-1-helper ()
    (let(
          (a (make-instance 'a-type))
          )
      (print (x a))
      (finish-output nil)
      (setf weak-a-1 (tg:make-weak-pointer a))
      (print (x (tg:weak-pointer-value weak-a-1)))
      (finish-output nil)
      ))

  (defun try-finalizer-1 ()
    (try-finalizer-1-helper)
    (print "a now out of scope")
    (finish-output nil)

    (print "running garbage collection")
    (finish-output nil)
    (tg:gc :full :verbose)
    (print "after garbage collection")
    (finish-output nil)

    (if (tg:weak-pointer-value weak-a-1)
      (print "weak-a-1 still defined")
      (print "weak-a-1 no longer defined")
      )
    'done
    )


;;--------------------------------------------------------------------------------
;; now add the finalizer call
;;
;; * (try-finalizer-2)
;; 
;; 23 
;; "hooking the finalizer" 
;; 23 
;; "a now out of scope" 
;; "running garbage collection" 
;; "being finalized" 
;; "after garbage collection" 
;; "weak-a-2 no longer defined" 
;;  DONE
;;
  (defvar weak-a-2)

  (defun being-finalized ()
    (print "'a' being finalized")
    (finish-output nil)
    (if (tg:weak-pointer-value weak-a-2)
      (print "inside finalizer weak-a-2 still defined")
      (print "inside finalizer weak-a-2 no longer defined")
      )
    )

  (defun try-finalizer-2-helper ()
    (let(
          (a (make-instance 'a-type))
          )
      (print (x a))
      (finish-output nil)

      (print "hooking the finalizer")
      (finish-output nil)
      (tg:finalize a #'being-finalized)

      (setf weak-a-2 (tg:make-weak-pointer a))
      (print (x (tg:weak-pointer-value weak-a-2)))
      (finish-output nil)
      ))

  (defun try-finalizer-2 ()
    (try-finalizer-2-helper)
    (print "'a' now out of scope")
    (finish-output nil)

    (print "running garbage collection")
    (finish-output nil)
    (tg:gc :full :verbose)
    (print "after garbage collection")
    (finish-output nil)

    (if (tg:weak-pointer-value weak-a-2)
      (print "in calling function weak-a-2 still defined")
      (print "in calling function weak-a-2 no longer defined")
      )
    'done
    )
