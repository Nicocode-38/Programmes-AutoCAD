; Auteur : Patrick_35
; Head : README
; Comm : Important de lire avant utilisation.
; Vers : Version

;Updates : mises a jours des versions avec coommentaire 

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

;;; Copyright (C) Patrick_35

(defun c:unverr(/ doc ent n sel)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
	n 0
  )
  (vla-startundomark doc)
  (if (ssget "_x" (list (cons 0 "VIEWPORT")))
    (progn
      (vlax-for ent (setq sel (vla-get-activeselectionset doc))
	(and (eq (vla-get-displaylocked ent) :vlax-true)
	     (setq n (1+ n))
	     (vla-put-displaylocked ent :vlax-false)
        )
      )
      (vla-delete sel)
    )
  )
  (princ (strcat "\n"  (itoa n) " fenêtre(s) de déverrouillée(s)"))
  (vla-endundomark doc)
  (princ)
)

(setq nom_lisp "UNVERR")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " chargé."))
    (princ (strcat "\n" nom_lisp ".LSP Chargé.....Tapez " nom_lisp " pour l'éxecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Chargé......Tapez " nom_lisp " pour l'éxecuter.")))
(setq nom_lisp nil)
(princ)