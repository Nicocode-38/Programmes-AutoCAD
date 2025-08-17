(defun c:PointsEtSuppPoly3D ()
  (vl-load-com)

  (setq ss (ssget '((0 . "POLYLINE")))) ; Filtre pour les POLYLINE3D
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        (if (= (vla-get-objectname obj) "AcDb3dPolyline")
          (progn
            ;; Parcours des sommets
            (setq vtx (entnext ent))
            (while (= (cdr (assoc 0 (entget vtx))) "VERTEX")
              (setq pt (cdr (assoc 10 (entget vtx))))
              (command "_.POINT" pt)
              (setq vtx (entnext vtx))
            )
            ;; Suppression de la poly3D
            (vla-delete obj)
          )
        )
        (setq i (1+ i))
      )
    )
    (prompt "\nAucune POLYLINE3D sélectionnée.")
  )
  (princ)
)
