(defun c:DeleteZeroLengthObjects ( / doc entities objtype obj len)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))) ;; Récupérer le document actif
  (setq entities (vla-get-ModelSpace doc)) ;; Obtenir les objets dans ModelSpace
  (setq count 0) ;; Compteur des objets supprimés
  
  ;; Parcours des objets dans ModelSpace
  (vlax-for obj entities
    (setq objtype (vla-get-ObjectName obj)) ;; Récupérer le type de l'objet
    
    ;; Vérifier les objets géométriques avec longueur de 0
    (cond
      ;; Si c'est une ligne
      ((= objtype "AcDbLine")
       (setq len (vla-get-Length obj)) ;; Calculer la longueur de la ligne
       (if (= len 0) ;; Si longueur = 0
         (progn
           (vla-delete obj) ;; Supprimer la ligne
           (setq count (1+ count)) ;; Incrémenter le compteur
         )
       )
      )

      ;; Si c'est un arc
      ((= objtype "AcDbArc")
       (setq len (vla-get-Length obj)) ;; Calculer la longueur de l'arc
       (if (= len 0) ;; Si longueur = 0
         (progn
           (vla-delete obj) ;; Supprimer l'arc
           (setq count (1+ count)) ;; Incrémenter le compteur
         )
       )
      )

      ;; Si c'est une polyligne
      ((= objtype "AcDbPolyline")
       (setq len (vla-get-Length obj)) ;; Calculer la longueur de la polyligne
       (if (= len 0) ;; Si longueur = 0
         (progn
           (vla-delete obj) ;; Supprimer la polyligne
           (setq count (1+ count)) ;; Incrémenter le compteur
         )
       )
      )

      ;; Si c'est un cercle (rayon 0)
      ((= objtype "AcDbCircle")
       (setq len (vla-get-Radius obj)) ;; Récupérer le rayon du cercle
       (if (= len 0) ;; Si rayon = 0
         (progn
           (vla-delete obj) ;; Supprimer le cercle
           (setq count (1+ count)) ;; Incrémenter le compteur
         )
       )
      )
      
      ;; Ajoute d'autres types d'objets géométriques si nécessaire
    )
  )

  ;; Afficher le résultat
  (if (> count 0)
    (princ (strcat "\n" (itoa count) " objets de longueur 0 supprimés."))
    (princ "\nAucun objet de longueur 0 trouvé.")
  )

  (princ)
)
