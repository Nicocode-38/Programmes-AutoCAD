; Auteur : Nicolas PISOT
; Head : Suppdblpts
; Comm : Supression des points identiques en n'en laissant qu'un

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:suppDblPts ( / ss idx ptList ent pt delList)
  (setq ss (ssget '((0 . "POINT")))) ; S�lectionne tous les points

  (if ss
    (progn
      (setq idx 0
            ptList '()
            delList '()
      )
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx)
              pt (cdr (assoc 10 (entget ent)))
        )
        (if (member pt ptList)
          (progn
            (setq delList (cons ent delList)) ; Dupliqu� ? � supprimer
          )
          (setq ptList (cons pt ptList)) ; Nouveau point ? on l�ajoute � la liste
        )
        (setq idx (1+ idx))
      )
      ;; Suppression des entit�s dupliqu�es
      (foreach e delList
        (entdel e)
      )
      (princ (strcat "\nNombre de points dupliqu�s supprim�s : " (itoa (length delList))))
    )
    (princ "\nAucun point trouv�.")
  )
  (princ)
)
