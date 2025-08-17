(defun c:DetachUnusedXrefs ( / acadApp doc blocks blk blkName usedRefs ss i entName)
  (vl-load-com)

  (setq acadApp (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadApp))
  (setq blocks (vla-get-Blocks doc))

  ;; Créer une liste des noms des Xrefs insérées
  (setq usedRefs '())
  (if (setq ss (ssget "_X" '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq entName (ssname ss i))
        (setq ent (vlax-ename->vla-object entName))
        (setq blkName (vla-get-name (vla-item blocks (vla-get-name ent))))
        (if (not (member blkName usedRefs))
          (setq usedRefs (cons blkName usedRefs))
        )
        (setq i (1+ i))
      )
    )
  )

  ;; Parcourir les blocs Xrefs et détacher ceux qui ne sont pas utilisés
  (vlax-for blk blocks
    (if (and
          (= :vlax-true (vla-get-IsXref blk))
          (not (member (vla-get-name blk) usedRefs))
        )
      (progn
        (vla-Detach blk)
        (princ (strcat "Xref détachée (non utilisée) : " (vla-get-name blk)))
      )
    )
  )

  (princ "\n? Nettoyage terminé : Xrefs non utilisées détachées.")
  (princ)
)
