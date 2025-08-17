(setq *color-counter* 1) ; variable globale pour suivre la couleur

(defun next-color ()
  (progn
    (if (or (null *color-counter*) (>= *color-counter* 255))
      (setq *color-counter* 1)
      (setq *color-counter* (+ *color-counter* 10))
    )
    *color-counter*
  )
)

(defun my-random (maxVal)
  (rem (getvar "DATE") maxVal)
)

(defun EnsureLayer (layName / acadApp acadDoc layTable lay color)
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq layTable (vla-get-Layers acadDoc))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply
               'vla-Item (list layTable layName)
             )
           ))
    ;; Si existe, renvoie le calque
    (vla-Item layTable layName)
    ;; Sinon crée et donne couleur incrémentale
    (progn
      (setq lay (vla-Add layTable layName))
      (setq color (next-color))
      (vla-put-Color lay color)
      lay
    )
  )
)

(defun c:EtiquetteReseau (/ ent entData pt1 pt2 midPt diam diamM netType labelText ang txtHeight entText
                            acadDoc layTable layPoly layText colorPoly colorText)

  (vl-load-com)

  ;; Sélection de la polyligne
  (setq ent (car (entsel "\nSélectionnez une polyligne 2D : ")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      ;; Entrée du diamètre
      (initget 7)
      (setq diam (getreal "\nEntrez le diamètre (en mm) : "))
      (setq diamM (/ diam 1000.0)) ; conversion en mètres

      ;; Entrée du type de réseau
      (setq netType (getstring t "\nEntrez le type de réseau (ex : EU, EP) : "))

      ;; Construction du texte
      (setq labelText (strcat netType " " (rtos diam 2 0)))

      ;; Points du premier segment
      (setq pt1 (vlax-curve-getPointAtParam ent 0))
      (setq pt2 (vlax-curve-getPointAtParam ent 1))

      ;; Calcul du milieu et de l'angle
      (setq midPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))
      (setq midPt (list (car midPt) (cadr midPt) 0.0)) ; Z = 0
      (setq ang (atan (- (cadr pt2) (cadr pt1)) (- (car pt2) (car pt1))))

      ;; Hauteur texte fixe 0.5
      (setq txtHeight 0.5)

      ;; Création du texte aligné
      (setq entText
        (entmakex
          (list
            '(0 . "TEXT")
            (cons 10 midPt)
            (cons 40 txtHeight)
            (cons 1 labelText)
            (cons 7 "Standard")
            (cons 50 ang) ; rotation selon le segment
            (cons 72 1) ; alignement centré horizontal
            (cons 73 2) ; alignement centré vertical
            (cons 11 midPt)
          )
        )
      )

      ;; Création ou récupération des calques
      (setq layPoly (EnsureLayer netType)) ; calque type réseau
      (setq layText (EnsureLayer (strcat "Etiquette - " netType))) ; calque etiquette

      ;; Déplacement polyligne vers calque type réseau
      (entmod (subst (cons 8 netType) (assoc 8 (entget ent)) (entget ent)))
      (entupd ent)

      ;; Déplacement texte vers calque etiquette
      (entmod (subst (cons 8 (strcat "Etiquette - " netType)) (assoc 8 (entget entText)) (entget entText)))
      (entupd entText)

      ;; Mise à jour de l’épaisseur de la polyligne
      (setq entData (entget ent))
      (setq entData (subst (cons 43 diamM) (assoc 43 entData) entData))
      (setq entData (subst (cons 44 diamM) (assoc 44 entData) entData))
      (entmod entData)
      (entupd ent)

      (prompt "\n? Étiquette créée, polyligne et calques mis à jour.")
    )
    (prompt "\n? Veuillez sélectionner une polyligne 2D.")
  )
  (princ)
)
