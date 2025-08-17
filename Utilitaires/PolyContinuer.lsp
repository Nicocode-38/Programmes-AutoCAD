; Auteur : Nicolas PISOT
; Head : Polycontinuer
; Comm : Cr�er un polyligne de deux points en continu.

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:polycontinuer ()
  (setq point1 (getpoint "\nS�lectionnez le premier point de la polyligne : "))
  (setq point2 (getpoint point1 "\nS�lectionnez le deuxi�me point de la polyligne : "))
  
  (while point1
    (command "_.PLINE" point1 point2 "")  ; Utilisation de la commande PLINE pour dessiner la polyligne
    (setq point1 (getpoint "\nS�lectionnez un autre premier point (ou appuyez sur Entr�e pour quitter) : "))
    (if point1
      (setq point2 (getpoint point1 "\nS�lectionnez un deuxi�me point : "))
    )
  )
  (princ "\nProcessus termin�.")
)

(princ "\nTapez 'polycontinuer' pour d�marrer.")
