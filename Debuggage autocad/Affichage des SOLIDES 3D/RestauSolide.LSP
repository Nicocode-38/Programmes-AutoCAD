(defun c:RestaurerSolides3D ( / pair edata)
  (if *Solides3D_CalquesOriginaux*
    (progn
      (foreach pair *Solides3D_CalquesOriginaux*
        (if (and (setq edata (entget (car pair))) (cdr pair))
          (progn
            (setq edata (subst (cons 8 (cdr pair)) (assoc 8 edata) edata))
            (entmod edata)
          )
        )
      )
      (princ (strcat "\n" (itoa (length *Solides3D_CalquesOriginaux*)) " solide(s) restauré(s)."))
    )
    (prompt "\nAucune donnée temporaire en mémoire. Lancez d'abord DeplacerSolides3D.")
  )
  (princ)
)
