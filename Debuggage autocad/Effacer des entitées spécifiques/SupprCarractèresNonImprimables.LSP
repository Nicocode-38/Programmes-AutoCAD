(defun c:CleanNonPrintableText ( / doc blocks tot ent txt cleanTxt)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-blocks doc))
  (setq tot 0)

  (vlax-for blk blocks
    ;; On ignore les blocs avec un nom de type xref (contenant "|")
    (if (not (wcmatch (vla-get-name blk) "*|*"))
      (vlax-for ent blk
        (if (member (vla-get-objectname ent) '("AcDbText" "AcDbMText"))
          (progn
            (setq txt (vla-get-textstring ent))

            ;; Nettoyage du texte (retirer tabulations, retours ligne, etc.)
            (setq cleanTxt (vl-string-trim " \t\n\r\u200B" txt)) ; supprime aussi U+200B (zero-width space)

            ;; Vérifier si la chaîne est vide après nettoyage
            (if (= cleanTxt "")
              (progn
                (vla-delete ent)
                (setq tot (1+ tot))
              )
            )
          )
        )
      )
    )
  )

  (princ (strcat "\n" (itoa tot) " entité(s) texte avec caractères non imprimables supprimée(s)."))
  (princ)
)
