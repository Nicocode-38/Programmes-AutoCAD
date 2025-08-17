(defun c:ExportLongueurs2D ( / ent obj calque liste-calques liste-obj fichier doc ms liste-calques-longueurs nomFichier cheminFichier calques-str longueur existant longueurs total )

  (princ "\nCliquez sur les objets à exporter (Entrée pour terminer) :")

  (setq liste-calques '())
  (setq liste-obj '())

  (while
    (progn
      ;; Affiche les calques déjà sélectionnés
      (if liste-calques
  (progn
    (princ (strcat "\nCalques déjà sélectionnés (" (itoa (length liste-calques)) ") : "))
    (foreach c (reverse liste-calques)
      (princ (strcat c " "))
    )
  )
)
      ;; Invite à sélectionner un objet
      (setq ent (entsel "\nSélectionnez un objet (Entrée pour terminer) : "))
    )
    (setq obj (vlax-ename->vla-object (car ent)))
    (setq calque (vla-get-layer obj))

    ;; Affiche immédiatement le calque
    (princ (strcat "\nObjet sur le calque : " calque))

    ;; Ajoute le calque à la liste si nouveau
    (if (not (member calque liste-calques))
      (setq liste-calques (cons calque liste-calques))
    )

    ;; Ajoute l'objet à la liste pour possible usage ultérieur
    (setq liste-obj (cons obj liste-obj))
  )

  ;; Si aucun calque sélectionné
  (if (null liste-calques)
    (progn
      (princ "\nAucun objet sélectionné. Fin du programme.")
      (exit)
    )
  )

  ;; Affiche la liste des calques sélectionnés
  (princ (strcat "\nCalques sélectionnés : " (vl-princ-to-string liste-calques)))

  ;; Demander le nom du fichier
  (initget 128)
  (setq nomFichier (getstring T "\nNom du fichier CSV à créer [par défaut : export_longueurs.csv] : "))
  (if (= nomFichier "")
    (setq nomFichier "export_longueurs.csv")
    (if (not (wcmatch (strcase nomFichier) "*.CSV"))
      (setq nomFichier (strcat nomFichier ".csv"))
    )
  )

  ;; Création du fichier
  (setq cheminFichier (strcat (getvar "dwgprefix") nomFichier))
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq liste-calques-longueurs '())
  (setq fichier (open cheminFichier "w"))
  (write-line "Calque;Longueur (m)" fichier)

  ;; Recherche des entités dans le modèle selon les calques
  (vlax-for obj ms
    (if (and
          (member (vla-get-layer obj) liste-calques)
          (or
            (eq (vla-get-objectname obj) "AcDbLine")
            (eq (vla-get-objectname obj) "AcDbPolyline")
            (eq (vla-get-objectname obj) "AcDbArc")
          )
        )
      (progn
        (setq calque (vla-get-layer obj))
        (setq longueur (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))

        (setq existant (assoc calque liste-calques-longueurs))
        (if existant
          (setq liste-calques-longueurs
                (subst (cons calque (cons longueur (cdr existant))) existant liste-calques-longueurs))
          (setq liste-calques-longueurs
                (cons (cons calque (list longueur)) liste-calques-longueurs))
        )
      )
    )
  )

  ;; Écriture du CSV
  (foreach item liste-calques-longueurs
    (setq calque (car item))
    (setq longueurs (cdr item))
    (setq total 0.0)
    (foreach l longueurs
      (setq total (+ total l))
      (write-line (strcat calque ";" (rtos l 2 2)) fichier)
    )
    (write-line (strcat "Total " calque ";" (rtos total 2 2)) fichier)
  )

  (close fichier)

  ;; Ouvre le fichier CSV automatiquement
  (startapp "explorer" cheminFichier)

  ;; Alerte finale
  (setq calques-str (apply 'strcat (mapcar '(lambda (c) (strcat c "\n")) (reverse liste-calques))))
  (alert (strcat "Export terminé !\n\nCalques exportés :\n\n" calques-str))
  (princ (strcat "\n? Export terminé. Fichier créé : " cheminFichier))
  (princ)
)
