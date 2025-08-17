; Auteur : Nicolas PISOT
; Head : CoteAlign
; Comm : Cote alignée améliorée avec interrecations utilisateur

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
      (setq entName (getstring T "\nEntrez le nom de l'élément : "))
      
      ;; Suffixe avec "m" en valeur par défaut
      (setq suffix (getstring T "\nEntrez le suffixe (par défaut: m) : "))
      (if (= suffix "") (setq suffix "m"))

      (cond
        ;; Mode 1 : deux points
        ((eq mode "1")
         (setq pt1 (getpoint "\nPremier point de la cote : "))
         (setq pt2 (getpoint pt1 "\nDeuxième point de la cote : "))
        )

        ;; Mode 2 : objet sélectionné
        ((eq mode "2")
         (setq ent (car (entsel "\nSélectionnez un objet à coter : ")))
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
           (progn (prompt "\nAucun objet sélectionné.") (exit))
         )
        )
      )

      ;; Création de la cote alignée
      (command "_.DIMALIGNED" "_non" pt1 "_non" pt2 pause)
      (setq dim (entlast))

      ;; Appliquer le texte personnalisé et forcer l'alignement
      (if dim
        (progn
          (setq dimData (entget dim))
          (setq dimData (subst (cons 1 (strcat entName " <> " suffix)) (assoc 1 dimData) dimData))
          (setq dimData (subst (cons 62 0) (assoc 62 dimData) dimData)) ; couleur par calque
          (setq dimData (subst (cons 72 0) (assoc 72 dimData) dimData)) ; alignement horizontal centré
          (setq dimData (subst (cons 73 0) (assoc 73 dimData) dimData)) ; vertical centré
          (entmod dimData)
          (entupd dim)
        )
      )
    )
    (prompt "\nCommande annulée ou entrée invalide.")
  )
  (princ)
)
