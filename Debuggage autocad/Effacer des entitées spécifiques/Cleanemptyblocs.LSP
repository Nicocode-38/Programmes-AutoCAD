(defun c:CleanEmptyBlocks ( / doc blocks blk name count ss i ent)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  (setq count 0)

  (vlax-for blk blocks
    (setq name (vla-get-Name blk))
    (if (and
          (= (vla-get-Count blk) 0)                         ; Bloc vide
          (not (wcmatch name "*`**"))                      ; Pas un bloc système
          (= (vla-get-IsXRef blk) :vlax-false)             ; Pas une XREF
          (= (vla-get-IsLayout blk) :vlax-false)           ; Pas un espace papier
        )
      (progn
        ;; Supprimer toutes les insertions de ce bloc
        (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 name))))
        (if ss
          (progn
            (setq i 0)
            (while (< i (sslength ss))
              (setq ent (ssname ss i))
              (entdel ent)
              (setq i (1+ i))
            )
          )
        )
        ;; Supprimer la définition du bloc
        (vla-delete blk)
        (setq count (1+ count))
      )
    )
  )

  (if (> count 0)
    (princ (strcat "\nBlocs vides supprimés (insertions + définition) : " (itoa count)))
    (princ "\nAucun bloc vide supprimé.")
  )
  (princ)
)
