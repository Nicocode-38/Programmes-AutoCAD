(defun c:ReloadAllXrefs ()
  (vl-load-com)
  (setq acadApp (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadApp))
  (setq blocks (vla-get-Blocks doc))

  (vlax-for blk blocks
    (if (= :vlax-true (vla-get-IsXref blk))
      (progn
        (setq result
          (vl-catch-all-apply
            '(lambda ()
               (vla-Reload blk)
             )
          )
        )
        (if (vl-catch-all-error-p result)
          (princ (strcat "\n[ERREUR] Impossible de recharger : " (vla-get-name blk)))
          (princ (strcat "\nRechargée : " (vla-get-name blk)))
        )
      )
    )
  )

  (princ "\nTentative de rechargement terminée.")
  (princ)
)


