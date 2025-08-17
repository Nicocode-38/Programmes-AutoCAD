; Auteur : Nicolas PISOT
; Head : chemincourt
; Comm : Raccorde avec une poly3D tous les points s�lectionn�s en prennant � chaque segment le point le plus proche du pr�c�dent

;=========================================================
;=Version Fonctionelle :				 	 =
;=Autocad MAP2024 - 2025					 =
;=Merci de regarder la Doc avant utilisation du programme		 =
;=========================================================

(defun c:cheminCourt (/ pStart pEnd selPoints ptsList i ent pt current nextPt minDist path)
  ;; S�lection du point de d�part
  (setq pStart (getpoint "\nS�lectionnez le point de d�part : "))
  ;; S�lection du point d'arriv�e
  (setq pEnd (getpoint "\nS�lectionnez le point d'arriv�e : "))
  
  ;; S�lection des points interm�diaires
  (princ "\nS�lectionnez les points interm�diaires (objets de type POINT) : ")
  (setq selPoints (ssget '((0 . "POINT"))))

  ;; R�cup�ration des points dans une liste
  (setq ptsList '())
  (if selPoints
    (progn
      (setq i 0)
      (while (< i (sslength selPoints))
        (setq ent (ssname selPoints i))
        (setq pt (cdr (assoc 10 (entget ent))))
        ;; On force Z = 0 pour la 2D
        (setq pt (list (car pt) (cadr pt) 0.0))
        (setq ptsList (cons pt ptsList))
        (setq i (1+ i))
      )
    )
  )

  ;; Ajouter le point final � la liste des points � visiter
  (setq pEnd (list (car pEnd) (cadr pEnd) 0.0))
  (setq ptsList (append ptsList (list pEnd)))

  ;; Initialiser le point courant et le chemin
  (setq current (list (car pStart) (cadr pStart) 0.0))
  (setq path (list current))

  ;; Boucle de recherche gloutonne
  (while ptsList
    (setq nextPt (car ptsList))
    (setq minDist (distance current nextPt))
    (foreach pt ptsList
      (if (< (distance current pt) minDist)
        (progn
          (setq nextPt pt)
          (setq minDist (distance current pt))
        )
      )
    )
    ;; Ajouter au chemin
    (setq path (append path (list nextPt)))
    ;; Retirer le point choisi de la liste
    (setq ptsList (vl-remove nextPt ptsList))
    ;; D�finir le nouveau point courant
    (setq current nextPt)
  )

  ;; V�rification : affichage du nombre de points
  (princ (strcat "\nNombre de points dans le chemin : " (itoa (length path))))

  ;; Tracer la polyligne
  (command "_.PLINE")
  (foreach pt path
    (command (list (car pt) (cadr pt)))
  )
  (command "") ; fin de la polyligne

  (princ "\nChemin trac� correctement.")
  (princ)
)
