(defun c:ConcaveHullFromPoly3D ( / ss alpha pts neighbors plinePts )

  (defun distance (p1 p2)
    (sqrt (+ (expt (- (car p2) (car p1)) 2)
             (expt (- (cadr p2) (cadr p1)) 2)))
  )

  (defun get-poly3d-vertices (e)
    (setq pts '())
    (setq ent e)
    (while (setq ent (entnext ent))
      (setq data (entget ent))
      (if (= (cdr (assoc 0 data)) "VERTEX")
        (setq pts (cons (list (car (cdr (assoc 10 data)))
                              (cadr (cdr (assoc 10 data)))) pts))
      )
    )
    pts
  )

  (setq ss (ssget '((0 . "POLYLINE")))) ;; Sélection des poly3D
  (if ss
    (progn
      (setq alpha (getreal "\nDistance seuil (alpha) : "))
      (setq pts '())
      
      ;; Extraire les sommets de chaque poly3D
      (repeat (sslength ss)
        (setq e (ssname ss (setq i (if i (1+ i) 0))))
        (setq pts (append pts (get-poly3d-vertices e)))
      )

      (setq neighbors '())

      ;; Trouver les voisins proches
      (foreach p pts
        (foreach q pts
          (if (and (not (equal p q)) (< (distance p q) alpha))
            (setq neighbors (cons (list p q) neighbors))
          )
        )
      )

      ;; Extraire les points pour polyligne
      (setq plinePts (mapcar 'car neighbors))

      ;; Dessiner la polyligne
      (command "_.PLINE")
      (foreach pt plinePts
        (command pt)
      )
      (command "") ;; Termine la polyligne
    )
    (prompt "\nAucune poly3D sélectionnée.")
  )
  (princ)
)
