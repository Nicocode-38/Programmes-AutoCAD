(defun c:CoteLongueurPL ( / ent obj longueur pt1 pt2 dimEnt dimObj nomElem texteCote)
  (vl-load-com)
  (setq ent (car (entsel "\nSélectionnez une polyligne : ")))
  (if (and ent (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "POLYLINE")))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq longueur (vla-get-length obj))

      ;; ?? Demande du nom de l'élément
      (setq nomElem (getstring T "\nNom de l'élément : "))

      (princ "\nSpécifiez le premier point de la cote : ")
      (setq pt1 (getpoint))
      (if pt1
        (progn
          (setq pt2 (getpoint pt1 "\nSpécifiez le deuxième point : "))
          (if pt2
            (progn
              (command "_DIMALIGNED" pt1 pt2 pause)
              (setq dimEnt (entlast))
              (if (and dimEnt (= (cdr (assoc 0 (entget dimEnt))) "DIMENSION"))
                (progn
                  (setq dimObj (vlax-ename->vla-object dimEnt))

                  ;; ?? Création du texte personnalisé
                  (setq texteCote
                    (strcat
                      "Longueur développée de \"" nomElem "\" : "
                      (rtos longueur 2 2)
                      " M"
                    )
                  )

                  ;; ?? Application du texte à la cote
                  (vla-put-TextOverride dimObj texteCote)

                  (prompt (strcat "\n" texteCote))
                )
                (prompt "\nÉchec de la création de la cote.")
              )
            )
            (prompt "\nPoint 2 invalide.")
          )
        )
        (prompt "\nPoint 1 invalide.")
      )
    )
    (prompt "\nObjet sélectionné invalide. Veuillez sélectionner une polyligne.")
  )
  (princ)
)
