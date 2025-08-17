(defun c:CleanEmptyLayouts ( / doc layouts layout count modelSpace viewports ent)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layouts (vla-get-Layouts doc))
  (setq count 0)

  ;; Boucle sur tous les layouts du dessin
  (vlax-for layout layouts
    (progn
      ;; Récupérer le bloc "ModelSpace" pour le layout actuel
      (setq modelSpace (vla-get-Block layout))

      ;; Compter le nombre de viewports dans le layout
      (setq viewports 0)
      (vlax-for ent modelSpace
        (if (= "AcDbViewport" (vla-get-objectname ent)) ;; Vérifie si c'est une fenêtre de vue
          (setq viewports (1+ viewports))
        )
      )

      ;; Si aucune fenêtre de vue et aucun objet dans le layout, le supprimer
      (if (and (= viewports 0) (= (vla-get-Count modelSpace) 0)) ;; Pas d'objet, pas de viewport
        (progn
          (princ (strcat "\nPrésentation vide supprimée : " (vla-get-Name layout)))
          (vla-Delete layout)  ;; Supprimer le layout vide
          (setq count (1+ count))  ;; Compter les suppressions
        )
      )
    )
  )

  ;; Affichage du nombre de présentations supprimées
  (if (> count 0)
    (princ (strcat "\nNombre total de présentations vides supprimées : " (itoa count)))
    (princ "\nAucune présentation vide trouvée.")
  )
  (princ)
)



