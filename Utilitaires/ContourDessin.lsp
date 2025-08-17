; Auteur : Nicolas PISOT
; Head : ConvexHull3Dpoly
; Comm : Crée un contour autour de poly3Ds 
; File : ConvexHull3DPoly.lsp
; Vers : 1.0
; Date de mise à jour : 25/07/2025
; Update : amélioration de la robustesse du programme

;=========================================================
;Suivi des versions
;-
;V1 - 25/07/2025 - Première diffusion - fonctionelle
;V1.1 - 25/07/2025 - Amélioration de la robustesse du programme
;-
;=========================================================

;=========================================================
;=Version Fonctionelle :				 	                       =
;=Autocad MAP2024 - 2025					                       =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

;; Fonction my-acos : version compatible AutoLISP
(defun my-acos (x)
  ;; Clamp x dans [-1, 1] pour éviter les erreurs de racine négative
  (cond
    ((> x 1.0) (setq x 1.0))
    ((< x -1.0) (setq x -1.0))
  )
  (atan (sqrt (- 1 (* x x))) x)
)


;; Calcule l'angle entre deux vecteurs
(defun vector-angle (v1 v2)
  (setq dot (apply '+ (mapcar '* v1 v2)))
  (setq mag1 (sqrt (apply '+ (mapcar '* v1 v1))))
  (setq mag2 (sqrt (apply '+ (mapcar '* v2 v2))))
  (if (and (/= mag1 0) (/= mag2 0))
    (my-acos (/ dot (* mag1 mag2)))
    0
  )
)

(defun subtract-points (p1 p2)
  (mapcar '- p1 p2)
)

(defun leftmost-point (points)
  (car (vl-sort points (function (lambda (a b) (< (car a) (car b))))))
)


(defun convex-hull (pts / hull point start dir next minAngle angle done)
  ;; Jarvis March (Gift Wrapping)
  (if (< (length pts) 3)
    pts
    (progn
      (setq hull '())
      (setq start (leftmost-point pts))
      (setq point start)
      (setq dir '(0 -1)) ; direction initiale
      (setq done nil)

      (while (not done)
        (setq next nil)
        (setq minAngle pi)

        (foreach p pts
          (if (not (equal p point 1e-6))
            (progn
              (setq angle (vector-angle dir (subtract-points p point)))
              (if (< angle minAngle)
                (progn
                  (setq minAngle angle)
                  (setq next p)
                )
              )
            )
          )
        )

        ;; Si aucun point trouvé ou on revient au départ → fin
        (if (or (null next) (equal next start 1e-6))
          (setq done T)
          (progn
            (setq hull (cons next hull))
            (setq dir (subtract-points next point))
            (setq point next)
          )
        )
      )

      (reverse (cons start hull))
    )
  )
)

(defun c:ConvexHull3DPoly ( / ss i ent v pt allPoints hull plData)
  (setq allPoints '())

  (prompt "\nSélectionnez les POLYLINE 3D : ")
  (setq ss (ssget '((0 . "POLYLINE"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        ;; Vérifie que c'est une POLYLINE 3D
        (if (= (logand (cdr (assoc 70 (entget ent))) 8) 8)
          (progn
            (setq v (entnext ent))
            (while (and v (/= (cdr (assoc 0 (entget v))) "SEQEND"))
              (if (= (cdr (assoc 0 (entget v))) "VERTEX")
                (progn
                  (setq pt (cdr (assoc 10 (entget v))))
                  ;; Ajoute seulement X Y
                  (setq allPoints (cons (list (car pt) (cadr pt)) allPoints))
                )
              )
              (setq v (entnext v))
            )
          )
        )
        (setq i (1+ i))
      )

      ;; Calcul du convex hull
      (setq hull (convex-hull allPoints))

      ;; Création de la polyligne si possible
      (if (and hull (> (length hull) 2))
        (progn
          (setq plData
            (append
              (list
                (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 100 "AcDbPolyline")
                (cons 90 (length hull))
                (cons 70 1) ; Fermée
              )
              ;; Ajouter chaque sommet (code 10 . (X Y))
              (mapcar (function (lambda (pt) (cons 10 pt))) hull)
            )
          )
          (entmakex plData)
          (prompt "\n✅ Polyligne de contour (convex hull) créée.")
        )
        (prompt "\n⚠️ Impossible de créer un contour (pas assez de points).")
      )
    )
    (prompt "\n❌ Aucune POLYLINE 3D sélectionnée.")
  )
  (princ)
)
