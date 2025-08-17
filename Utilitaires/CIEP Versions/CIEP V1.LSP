(defun c:CIEP ( / ss layouts curspace layout)
  ;; Sélection des objets dans le layout courant
  (prompt "\nSélectionnez les objets à dupliquer dans tous les layouts : ")
  (setq ss (ssget))
  
  (if ss
    (progn
      ;; Mémoriser le layout courant
      (setq curspace (getvar "CTAB"))

      ;; Copier les objets avec COPYBASE depuis le layout courant
      (command "_.COPYBASE" '(0 0 0))
      (command ss "")
      
      ;; Récupérer tous les noms de layouts
      (setq layouts (layoutlist))
      
      ;; Parcourir chaque layout
      (foreach layout layouts
        (if (not (= layout curspace)) ; ne pas copier dans le layout d'origine
          (progn
            (command "_.LAYOUT" "_Set" layout) ; basculer sur le layout
            (command "_.PASTECLIP" '(0 0 0))   ; coller à la même position
          )
        )
      )

      ;; Retour au layout d’origine
      (command "_.LAYOUT" "_Set" curspace)

      (prompt "\nDuplication terminée dans tous les layouts.")
    )
    (prompt "\nAucun objet sélectionné.")
  )
  (princ)
)

