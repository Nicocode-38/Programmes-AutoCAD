; Auteur : Nicolas Pisot
; Head : InterPolylignes
; Comm : D�tecter si des polylignes sont s�cantes et donne les coordon�es de la coupure

;=========================================================
;=Version Fonctionelle :				 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================


(defun c:InterPolylignes ( / refEnt refObj ss i ent obj pts)
  (vl-load-com)

  ;; S�lection de la polyligne de r�f�rence
  (setq refEnt (car (entsel "\nS�lectionnez la polyligne de r�f�rence: ")))
  (setq refObj (vlax-ename->vla-object refEnt))

  ;; S�lection des autres polylignes
  (setq ss (ssget '((0 . "LWPOLYLINE")))) ; on peut �largir aux polylignes 2D ou 3D si besoin

  (if (and refObj ss)
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; �viter de comparer la r�f�rence avec elle-m�me
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
                  (setq j (+ j 3)) ; avancer de 3 coordonn�es (X Y Z)
                )
              )
              (princ (strcat "\nAucune intersection avec polyligne #" (itoa i)))
            )
          )
        )

        (setq i (1+ i))
      )
    )
    (princ "\nErreur : S�lection invalide.")
  )

  (princ)
)

