(defun c:CleanEmptyTexts ( / doc tot)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))  ; Récupère le document actif
  (setq tot 0)  ; Compteur pour le nombre de textes supprimés
  
  ;; Recherche des blocs
  (vlax-for blk (vla-get-Blocks doc)
    (or 
      (wcmatch (vla-get-name blk) "*|*")  ;; Ignore les blocs de type "model space" et autres
      (vlax-for ent blk  ;; Parcours tous les éléments du bloc
        (if (member (vla-get-objectname ent) '("AcDbText" "AcDbMText"))  ;; Si l'élément est un texte
          (progn
            (setq text-val (vla-get-textstring ent))  ;; Récupère le texte
            (if (wcmatch text-val " "  )  ;; Si le texte est composé uniquement d'espace (ou vide)
                (progn
                  (vla-delete ent)  ;; Supprime le texte vide
                  (setq tot (1+ tot))  ;; Incrémente le compteur
                )
            )
          )
        )
      )
    )
  )

  ;; Affiche le nombre de textes supprimés
  (if (> tot 0)
    (princ (strcat "\n" (itoa tot) " texte(s) vide(s) supprimé(s)."))
    (princ "\nAucun texte vide trouvé.")
  )
  (princ)
)
