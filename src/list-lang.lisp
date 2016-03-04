#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

  Γ accepts an access program, which is a symbol.  It translates this
  little access program into a lisp program for performming the access.
  
  Γ also accepts an iterator and a list of augments.  The iterator
  may be moved by the access program (which might be the whole point
  of the access program), and the args may be consumed by the access
  program.  If the args list is too short, it is padded with ∅.

  The access program consists of a series of short function names,
  as follows:

    r
    ru
    rw
    rut
    rwt

    d
    du
    dw

    it

    ∃
    ~∃
    ∀
    ~∀
    ∃n
    ~∃n
    ∀n
    ~∀n

    gr    
    grn
    grrm
    gd    
    gdn
    
    sr
    srn
    srrm

  These may be run together, or separated by any number of dashes. E.g.s

  rrr ; step right three times
  rdr ; step right (returns a first object) descend into the object, step right

  As per my "Towards a Better Understanding of CAR and CDR paper".

|#


;;--------------------------------------------------------------------------------
;; errors
;;
  (define-condition Γ-malformed-access-program (error)
    ((text :initarg :text :reader text)))

  (define-condition Γ-unrecognized-command (error)
    ((text :initarg :text :reader text)))

  ;; (define-condition Γ-premature-end-args (warning)
  ;;  ((text :initarg :text :reader text)))

;;--------------------------------------------------------------------------------
;; helpers
;;



;;--------------------------------------------------------------------------------
;; a letter per function parser, very simple
;;
  (defun parse (access-program it object arg-it)  ; access-program is a string
    (let*(
           arg
           (command-list (list 'list))
           (command-rm command-list)
           )
      (labels(
               (mk-fun (cmd arg-cnt)
                 (let*(
                        (fun `(,cmd ,it ,object))
                        (fun-rm (last fun))
                        )
                   (loop
                     (when (= 0 arg-cnt) (return fun))
                     (decf arg-cnt)
                     (if 
                       (r arg-it arg)
                       (grow-r-rm fun-rm arg)
                       (grow-r-rm fun-rm ∅)
                       )
                     ))))
        
        (loop
          (when (string-null access-program) (return-from parse (cdr command-list)))
          (case (elt access-program 0)
            (#\-              
              )

            (#\d
              (cond
                ((is-prefix access-program "du") 
                  (drop access-program 1)
                  (grow-r-rm command-rm (mk-fun 'du 1))
                  )
                ((is-prefix access-program "dw")
                  (drop access-program 1)
                  (grow-r-rm command-rm (mk-fun 'dw 1))
                  )
                (t
                  (grow-r-rm command-rm (mk-fun 'd 0))
                  )))
            )
          (drop access-program 1) ; no matter which case we drop 1
          )

        (cdr command-list)
        )))

  (defun test-parse-0 ()
    (equal
      (parse "ddd" 'it 'object ∅)
      '((d it object) (d it object) (d it object))
      ))
  (test-hook test-0-parse)

  (defun test-parse-1 ()
    (equal
      (parse "ddud" 'it 'object (L 11))
      '((d it object) (du it object 11) (d it object))
      ))
  (test-hook test-1-parse)

;;--------------------------------------------------------------------------------
;;             
  (defmacro Γ (access-program it object &rest args)
    (cond
      ((null access-program)
        (L 'list ∅)
        )
      ((not (symbolp access-program))
        (let ((message "access program must be a symbol"))
          (print message)
          (error 'Γ-malformed-program-symbol :text message)
          (L 'list ∅)
          ))
      (t
(pprint (L (string access-program) it object args))

        (let(
              (command-list (parse (string access-program) it object args))
              )
(pprint command-list)
          (cons 'progn command-list)
          ))))

