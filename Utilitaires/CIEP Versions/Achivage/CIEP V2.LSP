(defun c:CIEP V2( / dcl_id layouts curspace layout ss selected-layouts lst index)
  ;; Charger le fichier DCL
  (setq dcl_id (load_dialog "CIEP V2.dcl"))
  (if (not (new_dialog "ciep_dialog" dcl_id))
    (progn
      (prompt "\nErreur lors du chargement de l'interface.")
      (exit)
    )
  )

  ;; Obtenir la liste des layouts
  (setq layouts (layoutlist))

  ;; Remplir la liste dans la boîte de dialogue
  (start_list "layout_list")
  (mapcar 'add_list layouts)
  (end_list)

  ;; Afficher la boîte de dialogue
  (if (= (start_dialog) 1)
    (progn
      ;; Récupérer les indexes sélectionnés
      (setq lst (get_attr "layout_list"))
      (setq selected-layouts '())

      (setq index 0)
      (foreach layout layouts
        (if (member index (read lst))
          (setq selected-layouts (cons layout selected-layouts))
        )
        (setq index (+ index 1))
      )

      ;; Sélection des objets
      (prompt "\nSélectionnez les objets à dupliquer : ")
      (setq ss (ssget))

      (if ss
        (progn
          (setq curspace (getvar "CTAB"))
          (command "_.COPYBASE" '(0 0 0))
          (command ss "")

          ;; Parcourir les layouts sélectionnés
          (foreach layout (reverse selected-layouts) ; reverse pour garder l’ordre original
            (if (/= layout curspace)
              (progn
                (command "_.LAYOUT" "_Set" layout)
                (command "_.PASTECLIP" '(0 0 0))
              )
            )
          )
          ;; Retour au layout original
          (command "_.LAYOUT" "_Set" curspace)
          (prompt "\n? Duplication terminée.")
        )
        (prompt "\n?? Aucun objet sélectionné.")
      )
    )
    (prompt "\n? Action annulée par l'utilisateur.")
  )

  (unload_dialog dcl_id)
  (princ)
)
