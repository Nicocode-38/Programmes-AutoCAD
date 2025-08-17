(defun c:ExportLongueurs2D ( / selSet i ent obj calque liste-calques fichier doc ms liste-calques-longueurs nomFichier cheminFichier )

  (princ "\nSélectionnez un ou plusieurs objets appartenant aux calques que vous voulez exporter :")
  (setq selSet (ssget))
  (if (not selSet)
    (progn
      (princ "\nAucune sélection. Fin du programme.")
      (exit)
    )
  )

  ;; Extraire les calques à partir des objets sélectionnés
  (setq liste-calques '())
  (setq i 0)
  (while (< i (sslength selSet))
    (setq ent (ssname selSet i))
    (setq obj (vlax-ename->vla-object ent))
    (setq calque (vla-get-layer obj))
    (if (not (member calque liste-calques))
      (setq liste-calques (cons calque liste-calques))
    )
    (setq i (1+ i))
  )

  (princ (strcat "\nCalques sélectionnés : " (vl-princ-to-string liste-calques)))

  ;; Demander un nom de fichier (avec valeur par défaut)
  (initget 128)
  (setq nomFichier (getstring T "\nNom du fichier CSV à créer [par défaut : export_longueurs.csv] : "))
  (if (= nomFichier "")
    (setq nomFichier "export_longueurs.csv")
    ;; Ajouter extension si oubliée
    (if (not (wcmatch (strcase nomFichier) "*.CSV"))
      (setq nomFichier (strcat nomFichier ".csv"))
    )
  )

  ;; Définir chemin complet du fichier
  (setq cheminFichier (strcat (getvar "dwgprefix") nomFichier))

  ;; Initialisation
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq liste-calques-longueurs '())
  (setq fichier (open cheminFichier "w"))
  (write-line "Calque;Longueur (m)" fichier)

  ;; Parcourir les objets du dessin
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
        
        ;; Ajouter la longueur au calque dans la liste d'association
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

  ;; Écriture du CSV avec point-virgule comme séparateur
  (foreach item liste-calques-longueurs
    (setq calque (car item))
    (setq longueurs (cdr item))
    (setq total 0.0)
    (foreach l (reverse longueurs)
      (setq total (+ total l))
      (write-line (strcat calque ";" (rtos l 2 2)) fichier)
    )
    (write-line (strcat "Total " calque ";" (rtos total 2 2)) fichier)
  )

  (close fichier)
  (princ (strcat "\n? Export terminé. Fichier créé : " cheminFichier))
  (princ)
)
