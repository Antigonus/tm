

(let*(
      (tm-source (mk 'list-nd-tm {:tape {0 1 2 3 4}}))
      (tm10 (mk 'list-nd-tm {:tape {∅}}))
      (tm-sink (mk 'status-tm {:base tm10 :empty t}))
      )
  (∀* tm-source
    (λ(tm-source)
      (as tm-sink (expt (r tm-source) 2))
      ))
  (tm-print tm-sink)
  )

(defun square-worker (tm-source tm-sink)
  (as tm-sink (expt (r tm-source) 2))
  (s tm-source)
  )

(let*(
      (tm-source (mk-Natural))
      (tm10 (mk 'list-nd-tm {:tape {∅}}))
      (tm-sink (mk 'status-tm {:base tm10 :empty t}))
      )
  (dotimes (i 5) (square-worker tm-source tm-sink))
  (tm-print tm-sink)
  )
