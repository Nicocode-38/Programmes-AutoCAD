; Auteur : Nicolas PISOT
; Head : Selection des polylignes 2D et 3D en fonction de leur longueur 2D
; File : SelPolyLonhg2D.lsp
; Comm : Sélection des polylignes 2D et 3D en fonction de leur longueur 2D

;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2025 - 2026				                         =
;=Covadis 2026                                           =
;=Merci de regarder la Doc avant utilisation du programme=
;========================================================

(defun c:SelPolyLong2D ( / maxlen ss allss i ent ename len2d selset)
  (setq maxlen (getreal "\nLongueur 2D maximale: "))
  (if (not maxlen)
    (progn (princ "\nValeur invalide.") (exit))
  )
  ; Sélectionner toutes les polylignes et polylignes 3D
  (setq allss (ssget "X" '((0 . "LWPOLYLINE,POLYLINE,POLYLINE3D"))))
  (if (not allss)
    (progn (princ "\nAucune polyligne trouvée.") (exit))
  )
  (setq selset (ssadd))
  (setq i 0)
  (while (< i (sslength allss))
    (setq ename (ssname allss i))
    (setq ent (entget ename))
    ; Calculer la longueur 2D
    (setq len2d (vlax-curve-getDistAtParam ename (vlax-curve-getEndParam ename)))
    (if (< len2d maxlen) ; <-- modifié ici
      (setq selset (ssadd ename selset))
    )
    (setq i (1+ i))
  )
  (if (> (sslength selset) 0)
    (progn
      (sssetfirst nil selset)
      (princ (strcat "\nNombre d'éléments sélectionnés: " (itoa (sslength selset))))
    )
    (princ "\nAucune polyligne trouvée sous la longueur spécifiée.") ; <-- message adapté
  )
  (princ)
)

(princ "\nCommande: SelPolyLong2D")
