; Auteur : Nicolas PISOT
; Head : Polycontinuer
; Comm : Créer un polyligne de deux points en continu.

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:polycontinuer ()
  (setq point1 (getpoint "\nSélectionnez le premier point de la polyligne : "))
  (setq point2 (getpoint point1 "\nSélectionnez le deuxième point de la polyligne : "))
  
  (while point1
    (command "_.PLINE" point1 point2 "")  ; Utilisation de la commande PLINE pour dessiner la polyligne
    (setq point1 (getpoint "\nSélectionnez un autre premier point (ou appuyez sur Entrée pour quitter) : "))
    (if point1
      (setq point2 (getpoint point1 "\nSélectionnez un deuxième point : "))
    )
  )
  (princ "\nProcessus terminé.")
)

(princ "\nTapez 'polycontinuer' pour démarrer.")
