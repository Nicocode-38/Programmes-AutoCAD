; Auteur : Nicolas PISOT
; Head : CoteAlign
; Comm : Cote align�e am�lior�e avec interrecations utilisateur

;=========================================================
;=Version Fonctionelle :				 	                       =
;=Autocad MAP2024 - 2025					                       =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

(defun c:COTEALIGN (/ mode ent entName suffix pt1 pt2 entData dim dimData)
  (initget "1 2")
  (setq mode (getkword "\nChoisir le mode de cotation [1:DeuxPoints/2:Objet] : "))

  (if (and mode (or (eq mode "1") (eq mode "2")))
    (progn
      (setq entName (getstring T "\nEntrez le nom de l'�l�ment : "))
      
      ;; Suffixe avec "m" en valeur par d�faut
      (setq suffix (getstring T "\nEntrez le suffixe (par d�faut: m) : "))
      (if (= suffix "") (setq suffix "m"))

      (cond
        ;; Mode 1 : deux points
        ((eq mode "1")
         (setq pt1 (getpoint "\nPremier point de la cote : "))
         (setq pt2 (getpoint pt1 "\nDeuxi�me point de la cote : "))
        )

        ;; Mode 2 : objet s�lectionn�
        ((eq mode "2")
         (setq ent (car (entsel "\nS�lectionnez un objet � coter : ")))
         (if ent
           (progn
             (setq entData (entget ent))
             (cond
               ;; Ligne
               ((= (cdr (assoc 0 entData)) "LINE")
                (setq pt1 (cdr (assoc 10 entData)))
                (setq pt2 (cdr (assoc 11 entData)))
               )
               ;; Bloc
               ((= (cdr (assoc 0 entData)) "INSERT")
                (setq pt1 (cdr (assoc 10 entData)))
                (setq pt2 (list (+ (car pt1) 1.0) (+ (cadr pt1) 0.5) (caddr pt1)))
               )
               (T
                (prompt "\nType d'objet non pris en charge.")
                (exit)
               )
             )
           )
           (progn (prompt "\nAucun objet s�lectionn�.") (exit))
         )
        )
      )

      ;; Cr�ation de la cote align�e
      (command "_.DIMALIGNED" "_non" pt1 "_non" pt2 pause)
      (setq dim (entlast))

      ;; Appliquer le texte personnalis� et forcer l'alignement
      (if dim
        (progn
          (setq dimData (entget dim))
          (setq dimData (subst (cons 1 (strcat entName " <> " suffix)) (assoc 1 dimData) dimData))
          (setq dimData (subst (cons 62 0) (assoc 62 dimData) dimData)) ; couleur par calque
          (setq dimData (subst (cons 72 0) (assoc 72 dimData) dimData)) ; alignement horizontal centr�
          (setq dimData (subst (cons 73 0) (assoc 73 dimData) dimData)) ; vertical centr�
          (entmod dimData)
          (entupd dim)
        )
      )
    )
    (prompt "\nCommande annul�e ou entr�e invalide.")
  )
  (princ)
)
