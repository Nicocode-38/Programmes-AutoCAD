; Auteur : Nicolas PISOT
; Head : D�discr�tisation des Polylignes (V1)
; Comm : Permet de filtrer les angles arrondis discr�tis�s d'une poly2D par tol�rance de distances.
;	 Exemple : Si une valeur de 0.5 est rentr�e chaque sommet se situant � moins de 0.5m du pr�c�dent est supprim�.
;															;

;=========================================================
;=Version Fonctionelle :				 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

(defun c:SimplifyPoly (/ ent entData minDist pts newPts i lastKept current)
  (setq minDist (getreal "\nDistance minimum entre points conserv�s : "))
  (setq ent (car (entsel "\nS�lectionner une polyligne : ")))

  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq entData (entget ent))
      (setq pts '())

      ;; Extraire les sommets (codes 10)
      (foreach item entData
        (if (= (car item) 10)
          (setq pts (append pts (list (cdr item))))
        )
      )

      ;; Toujours garder le premier point
      (setq newPts (list (car pts)))
      (setq lastKept (car pts))
      (setq i 1)

      ;; Boucle sur les sommets suivants
      (while (< i (length pts))
        (setq current (nth i pts))
        (if (>= (distance lastKept current) minDist)
          (progn
            (setq newPts (append newPts (list current)))
            (setq lastKept current)
          )
        )
        (setq i (1+ i))
      )

      ;; Supprimer l�ancienne polyligne
      (entdel ent)

      ;; Recr�er la nouvelle polyligne
      (entmakex
        (append
          (list
            (cons 0 "LWPOLYLINE")
            (cons 100 "AcDbEntity")
            (cons 100 "AcDbPolyline")
            (cons 90 (length newPts))
            (cons 70 0) ; ouverte (modifiable)
          )
          (mapcar '(lambda (pt) (cons 10 pt)) newPts)
        )
      )
      (princ (strcat "\nPolyligne simplifi�e. Sommets conserv�s : " (itoa (length newPts))))
    )
    (princ "\nEntit� non valide (LWPOLYLINE attendue).")
  )
  (princ)
)
