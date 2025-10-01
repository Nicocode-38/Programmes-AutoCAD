; Auteur : Nicolas PISOT
; Head : Arccrl
; Comm : création d'un arc de cercle par centre rayon longueur

;=========================================================
;=Version Fonctionelle :				 	                       =
;=Autocad MAP2025 - 2026					                       =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

(defun c:ArcCRL (/ center rayon longueur angle startAngle pt1)
  (prompt "\n--- Création d'un arc par Centre, Rayon, Longueur et Angle de départ ---")

  ;; Saisie du centre
  (setq center (getpoint "\nSpécifiez le centre de l'arc : "))

  ;; Saisie du rayon
  (setq rayon (getreal "\nSpécifiez le rayon : "))

  ;; Saisie de la longueur de l'arc
  (setq longueur (getreal "\nSpécifiez la longueur de l'arc : "))

  ;; Saisie de l'angle de départ en degrés
  (setq startAngle (getangle "\nSpécifiez l'angle de départ (en radians) : "))

  ;; Vérification des valeurs
  (if (and center rayon longueur startAngle (> rayon 0) (> longueur 0))
    (progn
      ;; Calcul de l'angle en radians : L = r * θ => θ = L / r
      (setq angle (/ longueur rayon))

      ;; Créer l'arc
      (entmakex
        (list
          (cons 0 "ARC")
          (cons 10 center)               ; centre
          (cons 40 rayon)               ; rayon
          (cons 50 startAngle)          ; angle de départ
          (cons 51 (+ startAngle angle)) ; angle de fin
        )
      )
      (prompt (strcat "\nArc créé avec un angle de " (rtos angle 2 4) " radians à partir de " (rtos startAngle 2 4) " radians."))
    )
    (prompt "\nValeurs invalides. Veuillez recommencer.")
  )
  (princ)
)

