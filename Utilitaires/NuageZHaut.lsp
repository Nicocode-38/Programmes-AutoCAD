; Auteur : Nicolas PISOT
; Head : README
; Comm : Important de lire avant utilisation.

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:NUAGEZMAX ( / ss i e face ptList pt x y z key found newList delAnswer)

  (defun makeKey (pt)
    ;; Crée une clé XY arrondie sous forme de chaîne
    (strcat (rtos (car pt) 2 2) "_" (rtos (cadr pt) 2 2))
  )

  (defun getKeyXY (pt)
    ;; Retourne les X arrondis et Y arrondis
    (list (atof (rtos (car pt) 2 2)) (atof (rtos (cadr pt) 2 2)))
  )

  (setq ss (ssget '((0 . "3DFACE"))))
  (if ss
    (progn
      (setq ptList '())

      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))
        (setq face (entget e))

        ;; Lire jusqu’à 4 sommets (codes 10 à 13)
        (foreach code '(10 11 12 13)
          (setq pt (cdr (assoc code face)))
          (if pt
            (progn
              (setq key (getKeyXY pt))
              (setq z (caddr pt))
              (setq found nil)

              ;; Cherche si XY déjà présent
              (setq newList '())
              (foreach existing ptList
                (if (equal (list (car existing) (cadr existing)) key 1e-6)
                  (progn
                    (setq found T)
                    (if (> z (caddr existing))
                      (setq newList (cons (append key (list z)) newList))
                      (setq newList (cons existing newList))
                    )
                  )
                  (setq newList (cons existing newList))
                )
              )

              (if (not found)
                (setq newList (cons (append key (list z)) newList))
              )

              (setq ptList newList)
            )
          )
        )
        (setq i (1+ i))
      )

      ;; Créer les POINTs
      (foreach pt ptList
        (entmakex (list (cons 0 "POINT") (cons 10 pt)))
      )

      (princ (strcat "\nPoints créés : " (itoa (length ptList))))

      ;; Demande de suppression
      (initget "Oui Non") ; évite saisie invalide
      (setq delAnswer (getstring T "\nSupprimer les 3DFACE ? [Oui/Non] <Oui>: "))
      (if (or (equal delAnswer "") (wcmatch (strcase delAnswer) "OUI"))
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (entdel (ssname ss i))
            (setq i (1+ i))
          )
          (princ "\n3DFACE supprimées.")
        )
      )
    )
    (prompt "\nAucune 3DFACE sélectionnée.")
  )
  (princ)
)

