; Auteur : Nicolas Pisot
; Head : InterPolylignes
; Comm : Détecter si des polylignes sont sécantes et donne les coordonées de la coupure

;=========================================================
;=Version Fonctionelle :				 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================


(defun c:InterPolylignes ( / refEnt refObj ss i ent obj pts)
  (vl-load-com)

  ;; Sélection de la polyligne de référence
  (setq refEnt (car (entsel "\nSélectionnez la polyligne de référence: ")))
  (setq refObj (vlax-ename->vla-object refEnt))

  ;; Sélection des autres polylignes
  (setq ss (ssget '((0 . "LWPOLYLINE")))) ; on peut élargir aux polylignes 2D ou 3D si besoin

  (if (and refObj ss)
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; Éviter de comparer la référence avec elle-même
        (if (/= ent refEnt)
          (progn
            (setq pts (vlax-invoke refObj 'IntersectWith obj acExtendNone))
            (if pts
              (progn
                (princ (strcat "\nIntersections avec polyligne #" (itoa i) " :"))
                (setq j 0)
                (while (< j (length pts))
                  (princ (strcat "\n  Point: "
                                 (rtos (nth j pts) 2 3) ", "
                                 (rtos (nth (+ j 1) pts) 2 3) ", "
                                 (rtos (nth (+ j 2) pts) 2 3)))
                  (setq j (+ j 3)) ; avancer de 3 coordonnées (X Y Z)
                )
              )
              (princ (strcat "\nAucune intersection avec polyligne #" (itoa i)))
            )
          )
        )

        (setq i (1+ i))
      )
    )
    (princ "\nErreur : Sélection invalide.")
  )

  (princ)
)

