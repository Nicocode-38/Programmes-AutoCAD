(defun orientation (p q r)
  ;; Calcule l'orientation du triplet (p, q, r)
  ;; >0 = gauche, <0 = droite, 0 = alignés
  (- (* (- (car q) (car p)) (- (cadr r) (cadr p)))
     (* (- (car r) (car p)) (- (cadr q) (cadr p))))
)

(defun distance-squared (p1 p2)
  (+ (expt (- (car p1) (car p2)) 2)
     (expt (- (cadr p1) (cadr p2)) 2))
)

(defun sort-by-angle (points ref)
  (vl-sort points
    (function
      (lambda (a b)
        (setq o (orientation ref a b))
        (cond
          ((< o 0) nil) ; droite
          ((> o 0) T)   ; gauche
          ;; si alignés, garder le plus proche
          (T (< (distance-squared ref a) (distance-squared ref b)))
        )
      )
    )
  )
)

(defun graham-scan (pts)
  (if (< (length pts) 3)
    pts
    (progn
      ;; trouver le point le plus bas
      (setq ref (car (vl-sort pts (function (lambda (a b)
        (or (< (cadr a) (cadr b))
            (and (= (cadr a) (cadr b)) (< (car a) (car b)))))))))
      (setq sorted (sort-by-angle (vl-remove ref pts) ref))
      (setq hull (list ref (car sorted)))
      (foreach p (cdr sorted)
        (while (and (> (length hull) 1)
                    (<= (orientation (cadr hull) (car hull) p) 0))
          (setq hull (cdr hull)) ; enlever le dernier point
        )
        (setq hull (cons p hull))
      )
      (reverse hull)
    )
  )
)

(defun c:FastConvexHull3D ( / ss i ent v pt allPoints hull plData)
  (setq allPoints '())

  (prompt "\nSélectionnez les POLYLINE 3D : ")
  (setq ss (ssget '((0 . "POLYLINE"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        ;; Vérifie que c'est une 3D POLYLINE
        (if (= (logand (cdr (assoc 70 (entget ent))) 8) 8)
          (progn
            (setq v (entnext ent))
            (while (and v (/= (cdr (assoc 0 (entget v))) "SEQEND"))
              (if (= (cdr (assoc 0 (entget v))) "VERTEX")
                (setq pt (cdr (assoc 10 (entget v)))
                      allPoints (cons (list (car pt) (cadr pt)) allPoints))
              )
              (setq v (entnext v))
            )
          )
        )
        (setq i (1+ i))
      )

      (setq hull (graham-scan allPoints))

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
              (mapcar (function (lambda (pt) (cons 10 pt))) hull)
            )
          )
          (entmakex plData)
          (prompt "\n✅ Contour convex rapide généré !")
        )
        (prompt "\n⚠️ Pas assez de points pour créer un contour.")
      )
    )
    (prompt "\n❌ Aucune POLYLINE 3D sélectionnée.")
  )
  (princ)
)
