; Auteur : Patrick_35
; Head : SCPSCU
; Comm : Importation SCU
; Vers : -

;Updates : - 

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:cpscu(/ dbx dcl dcl_id doc fic lay lst n msgbox Ouvrir_dessin_dbx res sty)

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Message
  ;;;
  ;;;---------------------------------------------------------------

  (defun MsgBox (Titre Bouttons Message / Reponse WshShell)
    (vl-load-com)  
    (setq WshShell (vlax-create-object "WScript.Shell"))
    (setq Reponse  (vlax-invoke WshShell 'Popup Message 0 Titre (itoa Bouttons)))
    (vlax-release-object WshShell)
    Reponse
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Ouvrir un dessin via ObjectDbx
  ;;;
  ;;;---------------------------------------------------------------

  (defun Ouvrir_dessin_dbx(dwg / dbx doc lan rel)
    (and (setq dwg (findfile dwg))
      (progn
	(vlax-for doc (vla-get-documents (vlax-get-acad-object))
	  (and (eq (strcase (vla-get-fullname doc)) (strcase dwg))
	    (setq dbx doc lan T)
	  )
	)
	(and (not dbx)
	  (setq dbx (vlax-create-object (if (< (setq rel (atoi (getvar "ACADVER"))) 16)
					  "ObjectDBX.AxDbDocument"
					  (strcat "ObjectDBX.AxDbDocument." (itoa rel))
					)
		    )
	  )
	  (vla-open dbx dwg)
	)
      )
    )
    (list dbx lan)
  )

  ;;;---------------------------------------------------------------
  ;;;
  ;;; Routine principale
  ;;;
  ;;;---------------------------------------------------------------

  (vl-load-com)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (if (setq dcl (findfile "cpscu.dcl"))
    (if (setq fic (getfiled "Sélectionnez le dessin source" (getvar "dwgprefix") "dwg" 16))
      (if (setq dbx (Ouvrir_dessin_dbx fic))
	(progn
	  (setq dcl_id (load_dialog dcl) pos "0" doc (vla-get-usercoordinatesystems (vla-get-activedocument (vlax-get-acad-object))))
	  (vlax-for sty (vla-get-usercoordinatesystems (car dbx))
	    (setq lst (append lst (list (vla-get-name sty))))
	  )
	  (if lst
	    (progn
	      (new_dialog "cpscu" dcl_id "")
	      (start_list "cpscu")
		(mapcar 'add_list lst)
	      (end_list)
	      (set_tile "titre" "Importer des SCU V1.00")
	      (set_tile "cpscu" pos)
	      (mode_tile "cancel" 2)
	      (action_tile "cpscu"  "(setq pos $value)")
	      (action_tile "accept" "(done_dialog 1)")
	      (action_tile "cancel" "(done_dialog 0)")
	      (setq res (start_dialog))
	      (if (eq res 1)
		(progn
		  (while (not (eq pos ""))
		    (setq n   (read pos)
			  lay (append lay (list (vla-item (vla-get-usercoordinatesystems (car dbx)) (nth n lst))))
			  pos (substr pos (+ 2 (strlen (itoa n))) (strlen pos))
		    )
		  )
		  (vla-CopyObjects (car Dbx) (vlax-safearray-fill 
						(vlax-make-safearray vlax-vbObject (cons 0 (1- (length lay))))
						lay
					     )
				   doc
		  )
		  (msgbox "CPSCU" 48 "Importation des scu terminé.")
		)
	      )
	      (or (cadr dbx) (vlax-release-object (car dbx)))
	    )
	    (msgbox "CPSCU" 16 "Pas de scu à importer.")
	  )
	)
	(msgbox "CPSCU" 16 (strcat "Impossible d'ouvrir le fichier\n" fic))
      )
    )
    (msgbox "CPSCU" 16 "Fichier CPSCU.DCL introuvable.")
  )
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)

(setq nom_lisp "CPSCU")
(if app
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)