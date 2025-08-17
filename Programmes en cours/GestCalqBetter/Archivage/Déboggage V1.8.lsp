; Auteur : Nicolas PISOT
; Head : Renommer des calques Efficacement pour filtrage
; Comm : Permet de rennomer des calques efficacement depuis une interface DCL
; Vers : 1.6
; Date de mise à jour : 02/07/2025
; Update : Ajout de fonction créer plusieurs calques

;Suivi des versions

; V1 : Visualisation des calques dans l'interface DCL
; V1.1 : Ajouter un préfixe pour le filtrage des calques
; V1.2 Gestion des calques par couleur
; Fixed V1.2 Problème concernant le calque 0 et le calque courant réglé.
; V1.3 Ajout de dépcalqsolides et Restaurercalques.
; V1.4 Fixed - DepcalqsolidesBetter.
; V1.5 Fixed - Ajout De calques incrémentés
; V1.6 Ajout de la supression des calques vides
; V1.7 Ajout de la barre de recherche des calques
; V1.8 Supression geler calque; Ajout Filtrer par couleur;
; V1.8.1 Fixed - Fltrage - Ajout de calques incrémentés (couleur impossible pour raison à verifier) - Prefixe à fixer prochaine version

;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2024 - 2025				                         =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

;Début du programme
;Fonction utilitaire :
(defun obtenir-calques-selectionnes (nom-tile liste-calques / brut indices)
  ;; Récupère les indices sélectionnés sous forme de chaîne
  (setq brut (get_tile nom-tile)) ; Exemple : "0 2 3"
  ;; Convertit la chaîne en liste d'entiers
  (setq indices (mapcar 'atoi (read (strcat "(" brut ")"))))
  ;; Extrait les calques correspondants
  (mapcar '(lambda (i) (nth i liste-calques)) indices)
)
;=======================================================================================================================================================
(defun c:SupprimerCalquesVides ( / doc layers lay nom ss supprimés courant)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (setq courant (strcase (getvar "CLAYER")))
  (setq supprimés 0)

  (vlax-for lay layers
    (setq nom (vla-get-Name lay))

    ;; Conditions pour ignorer le calque
    (if (and
          (/= (strcase nom) "0")               ; Ne pas supprimer le calque 0
          (/= (strcase nom) courant)           ; Ni le calque courant
          (/= (vla-get-Lock lay) :vlax-true)   ; Ni verrouillé
          (not (wcmatch nom "*|*"))            ; Ni calque XREF
        )

      ;; Vérifier si le calque est vide (aucun objet associé)
      (progn
        (setq ss (ssget "X" (list (cons 8 nom))))
        (if (not ss)
          (progn
            (vl-catch-all-apply 'vla-Delete (list lay))
            (setq supprimés (1+ supprimés))
            (princ (strcat "\n??? Calque vide supprimé : " nom))
          )
        )
      )
    )
  )

  ;; Résultat final
  (if (> supprimés 0)
    (alert (strcat (itoa supprimés) " calque(s) vide(s) supprimé(s)."))
    (alert "? Aucun calque vide trouvé.")
  )

  (princ))

;========================================================================================================================================================================
(defun c:DeplacerSolides3D ( / ss i ent edata origCalque)
  (prompt "\nDéplacement des solides 3D vers le calque 'Solides3D' avec mémoire temporaire...")
  (setq *Solides3D_CalquesOriginaux* nil)
  (if (not (tblsearch "LAYER" "Solides3D"))
    (command "._-layer" "n" "Solides3D" "")
  )
  (setq ss (ssget "X" '((0 . "3DSOLID"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq edata (entget ent))
        (setq origCalque (cdr (assoc 8 edata)))
        (setq *Solides3D_CalquesOriginaux*
              (cons (cons ent origCalque) *Solides3D_CalquesOriginaux*))
        (setq edata (subst (cons 8 "Solides3D") (assoc 8 edata) edata))
        (entmod edata)
        (setq i (1+ i))))
    (prompt "\nAucun solide 3D trouvé."))
  (princ))
;=======================================================================================================================================================================
(defun c:CalquesManagerPro ( / dcl_id retour selection prefix prefixe_creation nb_str couleur_str nb couleur
                                chaine tryread selIndexes selectedLayers doc layerCol count skipped newname lay ss edata)
  (vl-load-com)

  ;; Chargement du DCL
  (setq dcl_file "calques.dcl")
  (if (not (findfile dcl_file))
    (progn (alert (strcat "Fichier DCL manquant : " dcl_file)) (exit)))
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "calques_dialog" dcl_id)) (exit))

  ;; Récupération des calques
  (setq allLayers '())
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq allLayers (cons (cdr (assoc 2 lay)) allLayers))
    (setq lay (tblnext "LAYER")))
  (setq layers (vl-sort allLayers '<))

  ;; Affichage initial
  (start_list "layers_list")
  (mapcar 'add_list layers)
  (end_list)

  ;; Actions
  (action_tile "layers_list" "(setq selection $value)")

  (action_tile "btn_prefixe"
    "(setq prefix (get_tile \"prefixe\"))
     (done_dialog 1)")

  (action_tile "btn_filtrer_nom"
    "(setq filtre (get_tile \"filtre_nom\"))
     (start_list \"layers_list\" 3)
     (mapcar 'add_list
       (vl-remove-if-not
         '(lambda (n)
            (wcmatch (strcase n) (strcat \"*\" (strcase filtre) \"*\")))
         layers))
     (end_list))")

  (action_tile "btn_filtrer_couleur"
    "(progn
       (setq couleur (atoi (get_tile \"filtre_couleur\")))
       (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
       (setq layerCol (vla-get-Layers doc))
       (setq filtered '())
       (vlax-for l layerCol
         (setq colObj (vla-get-TrueColor l))
         (if (= (vla-get-ColorIndex colObj) couleur)
           (setq filtered (cons (vla-get-Name l) filtered))))
       (start_list \"layers_list\" 3)
       (mapcar 'add_list filtered)
       (end_list))")

  (action_tile "btn_suppr_vides"
    "(progn
       (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
       (setq layersObj (vla-get-Layers doc))
       (setq currentLayer (getvar \"CLAYER\"))
       (setq deleted 0)
       (vlax-for l layersObj
         (setq lname (vla-get-Name l))
         (if (and (/= (strcase lname) \"0\")
                  (/= (strcase lname) (strcase currentLayer))
                  (= (vla-get-Lock l) :vlax-false)
                  (not (wcmatch lname \"*|*\")))
           (progn
             (setq ss (ssget \"X\" (list (cons 8 lname))))
             (if (not ss)
               (progn
                 (vl-catch-all-apply 'vla-Delete (list l))
                 (setq deleted (1+ deleted)))))))
       (if (> deleted 0)
         (alert (strcat (itoa deleted) \" calque(s) vide(s) supprimé(s).\")) 
         (alert \"Aucun calque vide trouvé.\")))")

  (action_tile "btn_deplacer_solides"
    "(setq *Solides3D_CalquesOriginaux* nil)
     (if (not (tblsearch \"LAYER\" \"Solides3D\"))
       (command \"._-layer\" \"n\" \"Solides3D\" \"\"))
     (setq ss (ssget \"X\" '((0 . \"3DSOLID\"))))
     (if ss
       (progn
         (setq i 0)
         (while (< i (sslength ss))
           (setq ent (ssname ss i))
           (setq edata (entget ent))
           (setq origLayer (cdr (assoc 8 edata)))
           (setq *Solides3D_CalquesOriginaux*
                 (cons (cons ent origLayer) *Solides3D_CalquesOriginaux*))
           (setq edata (subst (cons 8 \"Solides3D\") (assoc 8 edata) edata))
           (entmod edata)
           (setq i (1+ i)))
         (prompt \"\\n Solides déplacés vers 'Solides3D'.\"))
       (prompt \"\\n Aucun solide 3D trouvé.\"))")

  (action_tile "btn_restaurer"
    "(if *Solides3D_CalquesOriginaux*
       (progn
         (foreach pair *Solides3D_CalquesOriginaux*
           (if (and (setq edata (entget (car pair))) (cdr pair))
             (progn
               (setq edata (subst (cons 8 (cdr pair)) (assoc 8 edata) edata))
               (entmod edata))))
         (prompt \"\\n Solides restaurés.\"))
       (prompt \"\\n Aucune donnée temporaire.\"))")

  (action_tile "btn_renommer"
    "(progn
       (setq selection (get_list_tile \"layers_list\"))
       (setq prefix (get_tile \"prefixe\"))
       (if (or (null selection) (= prefix \"\"))
         (alert \"Veuillez sélectionner des calques et entrer un préfixe.\")
         (done_dialog 1)))")

  (action_tile "btn_reset"
    "(start_list \"layers_list\" 3)
     (mapcar 'add_list layers)
     (end_list)
     (set_tile \"filtre_nom\" \"\")
     (set_tile \"filtre_couleur\" \"\")")

  (action_tile "btn_creer_num"
    "(progn
       (setq prefixe_creation (get_tile \"prefixe_creation\"))
       (setq nb_str (get_tile \"nb_calques\"))
       (setq couleur_str (get_tile \"couleur_creation\"))
       (done_dialog 3))")

  (action_tile "cancel" "(done_dialog)")
  
  ;; Lancement du dialogue
  (setq retour (start_dialog))
  (unload_dialog dcl_id)
  (alert (strcat "retour = " (itoa retour)
               "\nprefixe_creation = " prefixe_creation
               "\nnb_str = " nb_str
               "\ncouleur_str = " couleur_str))

  ;; === Création de calques multiples ===
(if (= retour 3)
  (progn
    (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
    (setq layerCol (vla-get-Layers doc))
    (setq count 0 skipped 0)

    ;; Vérification des champs obligatoires
    (if (and prefixe_creation (/= prefixe_creation "")
             nb_str (/= nb_str "")
             couleur_str (/= couleur_str "")
             (not (wcmatch couleur_str "*[A-Za-z]*")))

      (progn
        ;; Conversion sécurisée
        (setq nb (atoi nb_str))
        (setq couleur_val (atoi couleur_str))

        ;; Validation de plage
        (if (and (> nb 0) (>= couleur_val 1) (<= couleur_val 256))
          (progn
            ;; Création couleur
            (setq colorObj (vlax-create-object "AutoCAD.AcCmColor.1"))
            (vl-catch-all-apply
              '(lambda () (vla-put-ColorIndex colorObj couleur_val)))

            ;; Boucle de création
            (repeat nb
              (setq nom (vl-string-trim " " (strcat prefixe_creation (itoa (1+ count)))))
              (if (tblsearch "LAYER" nom)
                (setq skipped (1+ skipped))
                (progn
                  (setq newLayer (vl-catch-all-apply 'vla-Add (list layerCol nom)))
                  (if (and newLayer (not (vl-catch-all-error-p newLayer)))
                    (progn
                      (vl-catch-all-apply
                        '(lambda () (vla-put-TrueColor newLayer colorObj)))
                      (setq count (1+ count)))
                    (setq skipped (1+ skipped))))))
            ;; Résumé
            (alert (strcat (itoa count) " calque(s) créé(s), " (itoa skipped) " ignoré(s).")))
          (alert "❌ Nombre ou couleur invalide (entre 1 et 256).")))
      (alert "❌ Veuillez remplir tous les champs correctement."))))

  ;; Renommage de calques sélectionnés
  (if (= retour 1)
    (progn
      (if (and selection (listp selection))
        (progn
          (setq selectedLayers (obtenir-calques-selectionnes "layers_list" layers))
            (progn
              (setq selectedLayers (mapcar '(lambda (i) (nth i layers)) selIndexes))
              (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
              (setq layerCol (vla-get-Layers doc))
              (setq count 0 skipped 0)
              (foreach lay selectedLayers
                (setq newname (strcat prefix lay))
                (if (tblsearch "LAYER" newname)
                  (setq skipped (1+ skipped))
                  (progn
                    (vl-catch-all-apply
                      '(lambda ()
                         (vla-put-Name (vla-Item layerCol lay) newname)))
                    (setq count (1+ count)))))
              (alert (strcat (itoa count) " calque(s) renommé(s), " (itoa skipped) " ignoré(s).")))
            (alert "❌ Indices invalides.")))
        (alert "❌ Aucune sélection valide."))))

  ;; Mise à jour graphique
  (command "._REGEN")
  (princ)
