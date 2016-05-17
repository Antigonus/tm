(LET (AD BD)
  (UNWIND-PROTECT 
    (PROGN 
      (SETQ AD (DUP A)) 
      (SETQ BD (DUP B)) 
      (L (R AD) (R BD))
      )
    (WHEN AD (DISENTANGLE AD))
    (WHEN BD (DISENTANGLE BD))
    ))
