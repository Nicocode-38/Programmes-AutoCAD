(defun c:UnloadAllXrefs ()
  (vl-load-com)
  (setq acadApp (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadApp))
  (setq blocks (vla-get-Blocks doc))

  (vlax-for blk blocks
    (if (= :vlax-true (vla-get-IsXref blk))
      (progn
        (vla-Unload blk)
        (princ (strcat "\nDéchargée : " (vla-get-name blk)))
      )
    )
  )

  (princ "\ Toutes les Xrefs ont été déchargées.")
  (princ)
)






