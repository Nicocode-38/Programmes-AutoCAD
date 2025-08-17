(defun c:CleanEmptyGroups ( / doc groups grp count)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq groups (vla-get-Groups doc))
  (setq count 0)

  (vlax-for grp groups
    (if (= (vla-get-Count grp) 0)  ; Groupe sans entités
      (progn
        (princ (strcat "\nGroupe vide supprimé : " (vla-get-Name grp)))
        (vla-Delete grp)
        (setq count (1+ count))
      )
    )
  )

  (if (> count 0)
    (princ (strcat "\nNombre total de groupes vides supprimés : " (itoa count)))
    (princ "\nAucun groupe vide trouvé.")
  )
  (princ)
)
