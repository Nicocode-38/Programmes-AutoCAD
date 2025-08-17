(defun c:CleanEmptyDictionaries ( / doc dicts dict count hasEntry)
  (vl-load-com)
  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dicts (vla-get-Dictionaries doc))
  (setq count 0)

  (vlax-for dict dicts
    (setq hasEntry nil)

    ;; Vérifie si le dictionnaire contient des entrées
    (vl-catch-all-apply
      (function
        (lambda ()
          (vlax-for entry dict
            (setq hasEntry T)
          )
        )
      )
    )

    ;; Si vide, essayer de le supprimer
    (if (not hasEntry)
      (progn
        ;; Vérifie si on peut obtenir le nom (sinon éviter l'erreur)
        (if (vlax-property-available-p dict 'Name)
          (princ (strcat "\nSuppression du dictionnaire vide : " (vla-get-Name dict)))
          (princ "\nSuppression d’un dictionnaire vide sans nom lisible.")
        )
        (vl-catch-all-apply 'vla-delete (list dict))
        (setq count (1+ count))
      )
    )
  )

  ;; Message final
  (if (> count 0)
    (princ (strcat "\nNombre total de dictionnaires vides supprimés : " (itoa count)))
    (princ "\nAucun dictionnaire vide trouvé.")
  )
  (princ)
)
