#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Generalized mk structure we use throughout the tm library.

|# 

(in-package #:tm)

;; #'mk makes a new instance within the tm library.  We can not have a different #'mk for
;; each instance type, because the instance doesn't exist yet. I.e. #'mk is not itself a
;; constructor.
;;
;; We have a separate function that is called by #'mk after the instance is made, called
;; '#init.  Each format definition, concrete type, will have associated with it an #'init
;; function.
;;
;; #'mk is to data what #'eval is to programs.
;;
;; mk should defines (➜no-alloc #'alloc-fail)
;; while init should define  (➜ok #'echo) (➜bad (λ()(error 'bad-init-value)))
;; init can have additional continuations and options
;;
;;
  (def-function-class init (instance value &optional ➜))

  (defun mk (type &optional ➜)
    (destructuring-bind
      (&key
        value
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (declare (ignore ➜no-alloc)) ; temporary, need to add the error handlers around make-instance
      (let(
            (instance (make-instance type))
            )
        (init instance value ➜) ; I put the init value in the call so that dispatch can key from it
        )))



