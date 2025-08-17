(defun c:CIEPV1.1 ( / ss layouts curspace layout exclude-layout)
  ;; Demander à l'utilisateur s'il veut exclure un layout
  (setq exclude-layout (getstring T "\nNom du layout à exclure (laisser vide pour aucun) : "))

  ;; Sélection des objets dans le layout courant
  (prompt "\nSélectionnez les objets à dupliquer dans tous les layouts : ")
  (setq ss (ssget))
  
  (if ss
    (progn
      (setq curspace (getvar "CTAB"))
      (command "_.COPYBASE" '(0 0 0))
      (command ss "")
      (setq layouts (layoutlist))

      (foreach layout layouts
        (if (and (/= layout curspace)
                 (or (= exclude-layout "") (/= layout exclude-layout)))
          (progn
            (command "_.LAYOUT" "_Set" layout)
            (command "_.PASTECLIP" '(0 0 0))
          )
        )
      )
      (command "_.LAYOUT" "_Set" curspace)
      (prompt "\n? Duplication terminée dans les layouts sélectionnés.")
    )
    (prompt "\n?? Aucun objet sélectionné.")
  )
  (princ)
)
