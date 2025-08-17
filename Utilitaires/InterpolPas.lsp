;Auth : Nicolas Pisot
;Head : Créer un point interpolé sur une poly3D par pointage;
;Comment : Une sorte de discrétisation de polyligne 3D permettant d'ajouter un nouveau sommet à un pas donné

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:3dinterpoly (/ ent entdata step pts currentpt nextpt dist dir numsteps i j interppt newpts)
  (prompt "\nSélectionnez une POLYLINE3D : ")
  (setq ent (car (entsel)))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "POLYLINE"))
    (progn
      (setq step (getreal "\nEntrez la distance entre les sommets interpolés (ex: 2.0) : "))
      (setq entdata (entget ent))
      (setq pts '())
      
      ;; Collecter les sommets
      (setq ent (entnext ent)) ; aller au premier VERTEX
      (while (= (cdr (assoc 0 (entget ent))) "VERTEX")
        (setq pts (append pts (list (cdr (assoc 10 (entget ent))))))
        (setq ent (entnext ent))
      )

      ;; Interpoler les sommets
      (setq newpts (list (car pts))) ; commencer avec le premier point
      (setq i 0)
      (while (< i (- (length pts) 1))
        (setq currentpt (nth i pts))
        (setq nextpt (nth (+ i 1) pts))
        (setq dist (distance currentpt nextpt))
        (setq dir (mapcar '(lambda (a b) (/ (- b a) dist)) currentpt nextpt))
        (setq numsteps (fix (/ dist step)))

        (setq j 1)
        (while (<= j numsteps)
          (setq interppt (mapcar '+ currentpt (mapcar '(lambda (x) (* x step j)) dir)))
          (setq newpts (append newpts (list interppt)))
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )

      ;; Créer une nouvelle 3DPOLYLINE
      (entmakex '((0 . "POLYLINE") (66 . 1) (70 . 8))) ; entête POLYLINE 3D
      (foreach pt newpts
        (entmakex (list '(0 . "VERTEX") '(70 . 32) (cons 10 pt)))
      )
      (entmakex '((0 . "SEQEND")))

      (prompt "\nNouvelle 3DPOLY créée avec sommets interpolés.")
    )
    (prompt "\nEntité invalide. Veuillez sélectionner une POLYLINE3D.")
  )
  (princ)
)
