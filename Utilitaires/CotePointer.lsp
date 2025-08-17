(defun c:AnnotPolyLength ( / ent obj len txt prefix p1 p2)
  (vl-load-com) ;; Toujours charger VLAX
  (setq ent (car (entsel "\nSélectionnez une polyligne : ")))
  (if (and ent (setq obj (vlax-ename->vla-object ent)))
    (progn
      (if (wcmatch (vla-get-objectname obj) "*Polyline")
        (progn
          ;; Obtenir la longueur
          (setq len (vla-get-length obj))

          ;; Entrée du préfixe texte
          (setq prefix (getstring T "\nEntrez un texte (ex: Longueur, Caniveau, etc.) : "))
          (if (= prefix "") (setq prefix "Longueur")) ; Valeur par défaut

          ;; Création du texte final
          (setq txt (strcat prefix " : " (rtos len 2 2) " m"))

          ;; Pointage du point de flèche et du point de texte
          (setq p1 (getpoint "\nCliquez le point de flèche (près de la polyligne) : "))
          (setq p2 (getpoint p1 "\nCliquez le point de texte (où placer le texte) : "))

          ;; Utilise la commande MLEADER avec le texte
          (command "_mleader" p1 p2 txt "")
        )
        (prompt "\nErreur : l'objet sélectionné n'est pas une polyligne.")
      )
    )
    (prompt "\nAucune entité sélectionnée.")
  )
  (princ)
)
