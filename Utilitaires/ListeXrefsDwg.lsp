; Auteur : Nicolas PISOT
; Head : Lister les XREF DWG dans le dessin actif
; Comm : -
; Vers : 1.0
; Date de mise à jour : 16/07/2025
; Update : Mise à jour pour inclure les XREF DWG

;Suivi des versions : 
;-
;V1 : Valide
;V1.1 : Ajout de la hauteur du texte par défaut
;V1.2 : Correction de la condition de vérification des XREF DWG

;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2024 - 2025				                         =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

;Début du programme
;==================================================
(defun c:ListeXrefsDWG ( / doc blkCol xrefNames blk name mtextObj insPt txtHeight)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blkCol (vla-get-Blocks doc))
  (setq xrefNames '())
  (vlax-for blk blkCol
    (if (= (vla-get-IsXref blk) :vlax-true)
      (progn
        (setq path (vla-get-Path blk))
        (if (and path (wcmatch (strcase path) "*.DWG"))
          (setq xrefNames (cons (vla-get-Name blk) xrefNames))
        )
      )
    )
  )
  (if xrefNames
    (progn
      (setq xrefNames (reverse xrefNames))
      (setq insPt (getpoint "\nPoint d'insertion du texte : "))
      (initget 6)
      (setq txtHeight (getreal "\nHauteur du texte : "))
      (if (not txtHeight) (setq txtHeight 2.5))
      (setq mtextObj
        (vla-AddMText
          (vla-get-PaperSpace doc)
          (vlax-3d-point insPt)
          200
          (apply 'strcat (mapcar '(lambda (n) (strcat n "\n")) xrefNames))
        )
      )
      (vla-put-Height mtextObj txtHeight)
      (princ (strcat "\n" (itoa (length xrefNames)) " XREF(s) DWG listée(s)."))
    )
    (alert "Aucune XREF DWG trouvée.")
  )
  (princ)
)
; Fin du programme
(princ "\nCommande ListeXrefsDWG chargée. Tapez ListeXrefsDWG pour l'exécuter.")
(princ)