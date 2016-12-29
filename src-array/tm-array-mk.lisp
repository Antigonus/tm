#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-array (tape-machine)())

;;--------------------------------------------------------------------------------
;;
  (defmethod tm-init ((instance tm-array) init-list)
    (when 
      (¬ init-list) 
      (error 'tm-mk-bad-init-type :text "tm-array requires an initializer")
      )
    (let(
          (init (car init-list))
          )
      (cond
        ((∧ 
           (¬ (cdr init-list)) 
           (arrayp init)
           (¬ (adjustable-array-p init))
           )
          (setf (tape instance) init)
          (setf (head instance) 0)
          instance
          )

        (t 
          (error 'tm-mk-bad-init-type)
          ))))


  ;;--------------------------------------------------------------------------------
  ;; see the mount for tm-array-adj-mk as it also mounts the fixed array



 
  
