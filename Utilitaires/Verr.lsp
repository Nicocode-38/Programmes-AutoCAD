;;;=================================================================
;;;
;;; VERR.LSP V2.00
;;;
;;; Verrouiller toutes les fen�tres des pr�sentations
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun c:verr(/ doc ent n sel)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object))
	n 0
  )
  (vla-startundomark doc)
  (if (ssget "_x" (list (cons 0 "VIEWPORT")))
    (progn
      (vlax-for ent (setq sel (vla-get-activeselectionset doc))
	(and (eq (vla-get-displaylocked ent) :vlax-false)
	     (setq n (1+ n))
	     (vla-put-displaylocked ent :vlax-true)
        )
      )
      (vla-delete sel)
    )
  )
  (princ (strcat "\n"  (itoa n) " fen�tre(s) de verrouill�e(s)"))
  (vla-endundomark doc)
  (princ)
)

(setq nom_lisp "VERR")
(if (/= app nil)
  (if (= (strcase (substr app (1+ (- (strlen app) (strlen nom_lisp))) (strlen nom_lisp))) nom_lisp)
    (princ (strcat "..." nom_lisp " charg�."))
    (princ (strcat "\n" nom_lisp ".LSP Charg�.....Tapez " nom_lisp " pour l'�xecuter.")))
  (princ (strcat "\n" nom_lisp ".LSP Charg�......Tapez " nom_lisp " pour l'�xecuter.")))
(setq nom_lisp nil)
(princ)