(defun c:CleanZombieDictionaries ( / doc dict count item key safe-key)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq dict (vla-get-Dictionaries doc))
  (setq count 0)

  (vlax-for item dict
    ;; Sécurise l'accès à la propriété Name
    (setq safe-key 
          (vl-catch-all-apply
            '(lambda () (vla-get-Name item))
          )
    )
    (if (and (not (vl-catch-all-error-p safe-key)) ; Vérifie pas d'erreur
             (wcmatch (strcase safe-key) "*ZOMBIE*")
        )
      (progn
        (princ (strcat "\nSuppression du dictionnaire zombie : " safe-key))
        (vla-delete item)
        (setq count (1+ count))
      )
    )
  )

  (if (= count 0)
    (princ "\nAucun dictionnaire zombie trouvé.")
    (princ (strcat "\nNombre de dictionnaires zombies supprimés : " (itoa count)))
  )
  (princ)
)
