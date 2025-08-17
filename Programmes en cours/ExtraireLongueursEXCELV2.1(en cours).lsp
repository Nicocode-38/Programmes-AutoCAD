(defun c:ExporterLongueursCSV ( / dcl_id dcl_fn calque_liste file_name selected_indices selected_layers result_table doc ms obj calque longueur existant longueurs total fichier)

  ;; DCL
  (setq dcl_fn (findfile "ExporterLongueurs.dcl"))
  (if (null dcl_fn)
    (progn (princ "\n? Fichier DCL manquant.") (exit))
  )
  (setq dcl_id (load_dialog dcl_fn))
  (if (not (new_dialog "ExporterLongueursDialog" dcl_id))
    (progn (princ "\n? Erreur de bo�te de dialogue.") (exit))
  )

  ;; Lister les calques
  (setq calque_liste '())
  (setq layer (tblnext "LAYER" T))
  (while layer
    (setq calque_liste (cons (cdr (assoc 2 layer)) calque_liste))
    (setq layer (tblnext "LAYER"))
  )
  (setq calque_liste (vl-sort calque_liste '<))

  ;; Remplir la bo�te de dialogue avec les calques
  (start_list "calques")
  (foreach nom calque_liste (add_list nom))
  (end_list)

  ;; D�finir l'action pour le bouton "ok"
  (action_tile "ok" 
    "(setq selected_indices (get_tile \"calques\")) 
     (setq file_name (get_tile \"nom_fichier\"))
     (done_dialog 1)")

  ;; V�rifier si l'utilisateur a cliqu� sur OK
  (if (/= (start_dialog) 1)
    (progn
      (unload_dialog dcl_id) 
      (princ "\n? Annul�.") 
      (exit)
    )
  )

  ;; R�cup�rer les calques s�lectionn�s et le fichier
  (setq selected_layers '())
  (if (not (equal selected_indices "")) 
    (progn
      (foreach idx (mapcar 'atoi (split selected_indices ","))
        (setq selected_layers (cons (nth idx calque_liste) selected_layers))
      )
    )
  )

  (if (not selected_layers)
    (progn (princ "\n? Aucun calque s�lectionn�.") (exit))
  )

  ;; Pr�parer l'�criture du fichier CSV
  (setq file_name (ensure-csv file_name))
  (ensure-folder file_name)

  ;; Pr�parer l'�criture
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq ms (vla-get-ModelSpace doc))
  (setq result_table '())
  (setq fichier (open file_name "w"))
  (write-line "Calque;Longueur (m)" fichier)

  ;; Parcourir les objets dans le mod�le
  (vlax-for obj ms
    (if (and
          (member (vla-get-layer obj) selected_layers)
          (is-curve? obj)
        )
      (progn
        (setq calque (vla-get-layer obj))
        (setq longueur (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
        (setq existant (assoc calque result_table))
        (if existant
          (setq result_table
                (subst (cons calque (cons longueur (cdr existant))) existant result_table))
          (setq result_table
                (cons (cons calque (list longueur)) result_table))
        )
      )
    )
  )

  ;; �criture CSV
  (foreach item result_table
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

  ;; Ouvre le fichier dans l'explorateur
  (startapp "explorer" file_name)

  (alert (strcat "? Export termin� !\n\nFichier :\n" file_name))
  (princ (strcat "\n? Export CSV termin� : " file_name))
  (princ)
)
