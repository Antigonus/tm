#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-binner-0 ()
  (let(
        (tm-src (mk-tm 'list [2 21 12 42 43 47]))
        (tm-dsts (mk-tm 'list {(mk-tm 'list) (mk-tm 'list)}))
        )
    (labels(
             (op (object) (if (evenp object) 0 1))
             (worker (cont-ok cont◨) 
               (binner tm-src tm-dsts #'op {cont-ok cont◨})
               )
             )
      (⟳-work #'worker)
      )

    (let(
          (bin-0 (cdr (tape (r-index tm-dsts 0))))
          (bin-1 (cdr (tape (r-index tm-dsts 1))))
          )
      ;;(print {"bin-0" bin-0})
      ;;(print {"bin-1" bin-1})
      (∧
        (equal bin-0 [2 12 42])
        (equal bin-1 [21 43 47])
        ))))


(test-hook test-binner-0)
    
          
