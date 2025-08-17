(defun c:DeplacerSolides3D ( / ss i ent edata origCalque)
  (prompt "\nDéplacement des solides 3D vers le calque 'Solides3D' avec mémoire temporaire...")

  ;; Réinitialiser la liste globale
  (setq *Solides3D_CalquesOriginaux* nil)

  ;; Créer calque s'il n'existe pas
  (if (not (tblsearch "LAYER" "Solides3D"))
    (command "._-layer" "n" "Solides3D" "")
  )

  ;; Sélection des solides
  (setq ss (ssget "X" '((0 . "3DSOLID"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq edata (entget ent))
        (setq origCalque (cdr (assoc 8 edata)))

        ;; Enregistrer dans la liste globale
        (setq *Solides3D_CalquesOriginaux*
              (cons (cons ent origCalque) *Solides3D_CalquesOriginaux*))

        ;; Déplacer vers Solides3D
        (setq edata (subst (cons 8 "Solides3D") (assoc 8 edata) edata))
        (entmod edata)

        (setq i (1+ i))
      )
      (princ (strcat "\n" (itoa (sslength ss)) " solide(s) déplacé(s) vers Solides3D."))
    )
    (prompt "\nAucun solide 3D trouvé.")
  )
  (princ)
)

