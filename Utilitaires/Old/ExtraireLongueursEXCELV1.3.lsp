(defun c:ExportLongueurs2D ( / ent obj calque liste-calques liste-obj fichier doc ms liste-calques-longueurs nomFichier cheminFichier calques-str longueur existant longueurs total )

  (princ "\nCliquez sur les objets � exporter (Entr�e pour terminer) :")

  (setq liste-calques '())
  (setq liste-obj '())

  (while
    (progn
      ;; Affiche les calques d�j� s�lectionn�s
      (if liste-calques
  (progn
    (princ (strcat "\nCalques d�j� s�lectionn�s (" (itoa (length liste-calques)) ") : "))
    (foreach c (reverse liste-calques)
      (princ (strcat c " "))
    )
  )
)
      ;; Invite � s�lectionner un objet
      (setq ent (entsel "\nS�lectionnez un objet (Entr�e pour terminer) : "))
    )
    (setq obj (vlax-ename->vla-object (car ent)))
    (setq calque (vla-get-layer obj))

    ;; Affiche imm�diatement le calque
    (princ (strcat "\nObjet sur le calque : " calque))

    ;; Ajoute le calque � la liste si nouveau
    (if (not (member calque liste-calques))
      (setq liste-calques (cons calque liste-calques))
    )

    ;; Ajoute l'objet � la liste pour possible usage ult�rieur
    (setq liste-obj (cons obj liste-obj))
  )

  ;; Si aucun calque s�lectionn�
  (if (null liste-calques)
    (progn
      (princ "\nAucun objet s�lectionn�. Fin du programme.")
      (exit)
    )
  )

  ;; Affiche la liste des calques s�lectionn�s
  (princ (strcat "\nCalques s�lectionn�s : " (vl-princ-to-string liste-calques)))

  ;; Demander le nom du fichier
  (initget 128)
  (setq nomFichier (getstring T "\nNom du fichier CSV � cr�er [par d�faut : export_longueurs.csv] : "))
  (if (= nomFichier "")
    (setq nomFichier "export_longueurs.csv")
    (if (not (wcmatch (strcase nomFichier) "*.CSV"))
      (setq nomFichier (strcat nomFichier ".csv"))
    )
  )

  ;; Cr�ation du fichier
  (setq cheminFichier (strcat (getvar "dwgprefix") nomFichier))
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq liste-calques-longueurs '())
  (setq fichier (open cheminFichier "w"))
  (write-line "Calque;Longueur (m)" fichier)

  ;; Recherche des entit�s dans le mod�le selon les calques
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

  ;; �criture du CSV
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
  (alert (strcat "Export termin� !\n\nCalques export�s :\n\n" calques-str))
  (princ (strcat "\n? Export termin�. Fichier cr�� : " cheminFichier))
  (princ)
)
