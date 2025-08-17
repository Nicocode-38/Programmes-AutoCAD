; Auteur : Nicolas PISOT
; Head : Joindre de force deux polylignes
; File : JoindreforceV1.lsp
; Comm : Joindre deux polylignes en ajustant le sommet le plus proche
; Vers : 1.0
; Date de mise à jour : 18/07/2025
; Update : meilleire gestion de la jonction des polylignes

;=========================================================
;Suivi des versions
;-
;V1 - 17/07/2025 - Première diffusion - fonctionelle
;V1.1 - 18/07/2025 - Correction de la gestion de la jonction des polylignes
;V1.2 - 18/07/2025 - Meilleure robustesse du programme, si l'utilisateur sélectionne une polyligne vide, le programme ne plante pas.
;-
;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2024 - 2025				                         =
;=Covadis 2024 - 2025 18.0.0                             =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================
;Début Programme 
(defun c:SnapAndJoinPolylines ()
  (vl-load-com)

  ;; Sélection des deux polylignes
  (setq pl1 (car (entsel "\nSélectionnez la première polyligne: ")))
  (setq pl2 (car (entsel "\nSélectionnez la deuxième polyligne: ")))

  ;; Vérification des sélections
  (if (and pl1 pl2)
    (progn
      (setq obj1 (vlax-ename->vla-object pl1))
      (setq obj2 (vlax-ename->vla-object pl2))

      ;; Vérification du type d'objet
      (if (and (= (vla-get-objectname obj1) "AcDbPolyline")
               (= (vla-get-objectname obj2) "AcDbPolyline"))
        (progn
          ;; Extraction des coordonnées
          (defun get-coords (obj)
            (vlax-get obj 'Coordinates)
          )
          (defun to-points (coords)
            (setq pts '())
            (setq i 0)
            (while (< i (length coords))
              (setq pts (append pts (list (list (nth i coords) (nth (+ i 1) coords)))))
              (setq i (+ i 2))
            )
            pts
          )

          (setq coords1 (get-coords obj1))
          (setq coords2 (get-coords obj2))
          (setq pts1 (to-points coords1))
          (setq pts2 (to-points coords2))

          ;; Recherche des points les plus proches
          (setq minDist nil)
          (setq idx1 0)
          (setq closestIdx1 0)
          (foreach p1 pts1
            (foreach p2 pts2
              (setq dist (distance (list (car p1) (cadr p1) 0) (list (car p2) (cadr p2) 0)))
              (if (or (not minDist) (< dist minDist))
                (progn
                  (setq minDist dist)
                  (setq closestP1 p1)
                  (setq closestP2 p2)
                  (setq closestIdx1 idx1)
                )
              )
            )
            (setq idx1 (+ idx1 1))
          )

          ;; Mise à jour du sommet
          (setq newCoords coords1)
          (setq newCoords
            (subst (car closestP2) (nth (* 2 closestIdx1) coords1) newCoords)
          )
          (setq newCoords
            (subst (cadr closestP2) (nth (+ (* 2 closestIdx1) 1) coords1) newCoords)
          )
          (vlax-put obj1 'Coordinates newCoords)

          ;; Join des polylignes
          (command "._PEDIT" pl1 "J" pl2 "" "")
          (princ "\nPolylignes ajustées et jointes avec succès !")
        )
        (princ "\nErreur : les entités sélectionnées ne sont pas des polylignes 2D.")
      )
    )
    (princ "\nErreur : vous devez sélectionner deux polylignes.")
  )
)
(princ "\nTapez 'SnapAndJoinPolylines' pour exécuter la commande.")
(princ)
; Fin du programme
