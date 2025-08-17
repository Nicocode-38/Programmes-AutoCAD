; Auteur : Nicolas PISOT
; Head : Etiquette Avant Plan pour Covadis
; Comm : Permet de rennomer des calques efficacement depuis une interface DCL
; Vers : 1.0
; Date de mise à jour : 16/07/2025
; Update : -

;Suivi des versions
;-
;V1 - 16/07/2025 - Création de la fonction etiquette avant plan covadis réseau

;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2024 - 2025				                         =
;=Covadis 2024 - 2025 18.0.0                             =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

;Début du programme
;==================================================

(defun c:CovadisEtiqRéseauAvantPlan ( / ss )
  (vl-load-com)
  (setq ss (ssget "X" '((0 . "COVALABEL"))))
  (if ss
    (progn
      (sssetfirst nil ss) ; Simule une sélection active
      (command "_.DRAWORDER" "_Front")
    )
    (alert "Aucune étiquette Covadis trouvée.")
  )
  (princ)
)
(princ "\nCovadis Etiquette Avant Plan chargée.")
(princ)