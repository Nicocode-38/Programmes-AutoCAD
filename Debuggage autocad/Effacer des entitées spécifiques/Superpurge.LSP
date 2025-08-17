(defun c:SuperPurge ( / doc purgeType noOpt get-names types-before types-after removed-items message)

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Détection de la langue
  (if (= (strcase (getvar "locale")) "FR")
    (progn (setq purgeType "Tout") (setq noOpt "N"))
    (progn (setq purgeType "All")  (setq noOpt "No"))
  )

  ;; Fonction pour récupérer les noms depuis une collection COM
  (defun get-names (collection)
    (setq names '())
    (vlax-for item collection
      (setq names (cons (vla-get-name item) names))
    )
    names
  )

  ;; Sauvegarder les états avant la purge
  (setq types-before
    (list
      (cons "Blocs" (get-names (vla-get-blocks doc)))
      (cons "Calques" (get-names (vla-get-layers doc)))
      (cons "Styles de texte" (get-names (vla-get-textstyles doc)))
      (cons "Types de ligne" (get-names (vla-get-linetypes doc)))
    )
  )

  ;; Lancer la purge deux fois
  (princ "\n--- Lancement de la SUPERPURGE ---")
  (repeat 2
    (command "-purge" purgeType "*" noOpt)
  )
  (princ "\n--- PURGE terminée ---")

  ;; Sauvegarder l'état après la purge
  (setq types-after
    (list
      (cons "Blocs" (get-names (vla-get-blocks doc)))
      (cons "Calques" (get-names (vla-get-layers doc)))
      (cons "Styles de texte" (get-names (vla-get-textstyles doc)))
      (cons "Types de ligne" (get-names (vla-get-linetypes doc)))
    )
  )

  ;; Comparaison pour trouver les éléments supprimés
  (setq removed-items '())
  (foreach pair types-before
    (setq key (car pair))
    (setq before (cdr pair))
    (setq after (cdr (assoc key types-after)))
    (foreach item before
      (if (not (member item after))
        (setq removed-items (cons (strcat key " : " item) removed-items))
      )
    )
  )

  ;; Créer un message de la liste des éléments supprimés
  (setq message "Éléments supprimés:\n")
  (if removed-items
    (foreach item removed-items
      (setq message (strcat message item "\n"))
    )
    (setq message "Aucun élément supprimé.")
  )

  ;; Afficher une boîte de message avec les éléments supprimés
  (alert message)

  (princ)
)

