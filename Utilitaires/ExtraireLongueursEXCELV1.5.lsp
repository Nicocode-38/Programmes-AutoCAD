; Auteur : Nicolas PISOT
; Head : ExtraireLongueursExcel
; Comm : Exporte selon la selecetion les entit�es s�lection�es avec leurs calques.

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================
(defun c:ExtraireLongueursExcel ( / selection obj calque liste-calques liste-obj fichier doc ms liste-calques-longueurs nomFichier cheminFichier calques-str longueur existant total-global count-global old-len old-count total count )

  (princ "\nS�lectionnez les objets � exporter (fen�tre ou clic multiple) :")

  ;; Initialisation
  (setq liste-calques '())
  (setq liste-obj '())

  ;; S�lection multiple
  (setq selection (ssget "_:L")) ; "_:L" permet une s�lection graphique standard

  (if selection
    (progn
      (setq i 0)
      (repeat (sslength selection)
        (setq obj (vlax-ename->vla-object (ssname selection i)))
        (setq calque (vla-get-layer obj))
        (if (not (member calque liste-calques))
          (setq liste-calques (cons calque liste-calques))
        )
        (setq liste-obj (cons obj liste-obj))
        (setq i (1+ i))
      )
    )
    (progn
      (princ "\nAucun objet s�lectionn�. Fin du programme.")
      (exit)
    )
  )

  ;; Affiche les calques s�lectionn�s
  (princ (strcat "\nCalques s�lectionn�s : " (vl-princ-to-string liste-calques)))

  ;; Demande le nom du fichier
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
  (write-line "Calque;Longueur (m);Nombre d'objets" fichier)

  ;; Parcours du mod�le
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

        ;; Mise � jour de la liste des longueurs et comptage
        (setq existant (assoc calque liste-calques-longueurs))
        (if existant
          (progn
            (setq old-len (nth 1 existant))
            (setq old-count (nth 2 existant))
            (setq liste-calques-longueurs
                  (subst (list calque (+ old-len longueur) (1+ old-count)) existant liste-calques-longueurs)))
          (setq liste-calques-longueurs
                (cons (list calque longueur 1) liste-calques-longueurs)))
      )
    )
  )

  ;; �criture des donn�es dans le fichier
  (foreach item liste-calques-longueurs
    (setq calque (nth 0 item))
    (setq total (nth 1 item))
    (setq count (nth 2 item))
    (write-line (strcat calque ";" (rtos total 2 2) ";" (itoa count)) fichier)
  )

  ;; Calcul et �criture des totaux globaux
  (setq total-global 0.0)
  (setq count-global 0)
  (foreach item liste-calques-longueurs
    (setq total-global (+ total-global (nth 1 item)))
    (setq count-global (+ count-global (nth 2 item)))
  )
  (write-line (strcat "TOTAL GLOBAL;" (rtos total-global 2 2) ";" (itoa count-global)) fichier)

  (close fichier)

  ;; Ouvre le fichier CSV
  (startapp "explorer" cheminFichier)

  ;; Alerte finale
  (setq calques-str (apply 'strcat (mapcar '(lambda (c) (strcat c "\n")) (reverse liste-calques))))
  (alert (strcat "Export termin� !\n\nCalques export�s :\n\n" calques-str))
  (princ (strcat "\n? Export termin�. Fichier cr�� : " cheminFichier))
  (princ)
)
