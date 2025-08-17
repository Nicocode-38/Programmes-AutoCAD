; Auteur : Nicolas PISOT
; Head : Renommer des calques Efficacement pour filtrage
; Comm : Permet de rennomer des calques efficacement depuis une interface DCL
; Vers : 1.6
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

;=========================================================
;=Version Fonctionelle de Test :			 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================
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

(defun c:CalquesUI ()
  (vl-load-com)
  (setq dcl_file "calques.dcl")
  (if (not (findfile dcl_file))
    (progn (alert (strcat "Fichier DCL manquant : " dcl_file)) (exit)))
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "calques_dialog" dcl_id)) (exit))

  ;; Collecte des calques
  (setq allLayers '())
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq allLayers (cons (cdr (assoc 2 lay)) allLayers))
    (setq lay (tblnext "LAYER")))
  (setq layers (vl-sort allLayers '<))
  (setq selection "" prefix "" prefixe_flag nil)

  ;; Affichage liste
  (start_list "layers_list")
  (mapcar 'add_list layers)
  (end_list)

  ;; Actions
  (action_tile "layers_list" "(setq selection $value)")
  (action_tile "btn_prefixe"
    "(progn (setq prefix (get_tile \"prefixe\")) (setq prefixe_flag T) (done_dialog 1))")
  (action_tile "btn_filtrer"
    "(progn
       (setq couleurStr (get_tile \"filtre_couleur\"))
       (if (and couleurStr (/= couleurStr \"\") (> (atoi couleurStr) 0) (<= (atoi couleurStr) 255))
         (progn
           (setq couleurACI (atoi couleurStr))
           (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
           (setq layerCol (vla-get-Layers doc))
           (setq current (getvar \"CLAYER\"))
           (setq needChange nil)
           (setq cible \"0\")
           (if (tblsearch \"LAYER\" cible)
             (progn
               (setq testLay (vla-Item layerCol cible))
               (if (and (vlax-write-enabled-p testLay)
                        (= (vla-get-Freeze testLay) :vlax-false)
                        (/= current cible))
                 (progn (setvar \"CLAYER\" cible) (setq needChange T)))))
           (vlax-for lay layerCol
             (vl-catch-all-apply
               '(lambda ()
                  (setq lname (vla-get-Name lay))
                  (setq layColorIndex (vla-get-ColorIndex (vla-get-TrueColor lay)))
                  (if (= layColorIndex couleurACI)
                    (vla-put-Freeze lay :vlax-false)
                    (if (and (/= lname cible) (vlax-write-enabled-p lay))
                      (vla-put-Freeze lay :vlax-true))))))
           (if needChange (setvar \"CLAYER\" current))
           (prompt (strcat \"\\n? Calques ACI \" (itoa couleurACI) \" visibles. Les autres sont gelés.\")))
         (alert \"Veuillez entrer une couleur ACI valide entre 1 et 255.\")))")
  (action_tile "btn_degele"
  "(progn
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
     (setq layerCol (vla-get-Layers doc))
     (vlax-for lay layerCol
       (vl-catch-all-apply
         '(lambda ()
            (if (vlax-write-enabled-p lay)
              (vla-put-Freeze lay :vlax-false)))))
     (setq layers (vl-sort allLayers '<))
     (start_list \"layers_list\" 3)
     (mapcar 'add_list layers)
     (end_list)
     (prompt \"\\n? Tous les calques ont été dégélés.\"))")
  (action_tile "btn_restaurer"
    "(progn
       (if *Solides3D_CalquesOriginaux*
         (progn
           (foreach pair *Solides3D_CalquesOriginaux*
             (if (and (setq edata (entget (car pair))) (cdr pair))
               (progn
                 (setq edata (subst (cons 8 (cdr pair)) (assoc 8 edata) edata))
                 (entmod edata))))
           (prompt (strcat \"\\n? \" (itoa (length *Solides3D_CalquesOriginaux*)) \" solide(s) restauré(s).\")))
         (prompt \"\\n?? Aucune donnée temporaire. Lancez d'abord Déplacer solides 3D.\")))")
  (action_tile "btn_deplacer" "(done_dialog 99)")
  (action_tile "btn_creer_series"
  "(progn
     (setq prefix (get_tile \"prefix_layer\"))
     (setq nbStr (get_tile \"count_layer\"))
     (if (and prefix (/= prefix \"\") nbStr (/= nbStr \"\"))
       (progn
         (setq count (atoi nbStr))
         (if (> count 0)
           (progn
             (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
             (setq layers (vla-get-Layers doc))
             (setq created 0 skipped 0)
             (repeat count
               (setq name (strcat prefix (itoa (1+ created))))
               (if (tblsearch \"LAYER\" name)
                 (setq skipped (1+ skipped))
                 (progn
                   (vla-add layers name)
                   (setq created (1+ created)))))
             (prompt (strcat \"\\n? \" (itoa created) \" calque(s) créé(s), \" (itoa skipped) \" déjà existant(s).\")))
           (alert \"Veuillez entrer un nombre supérieur à 0.\")))
       (alert \"Veuillez spécifier un nom et un nombre.\")))")
  (action_tile "btn_suppr_vides"
  "(progn
     (c:SupprimerCalquesVides)
     (prompt \"\\n Calques vides supprimés.\")
  )")
  ;; Lancement
  (setq retour (start_dialog))
  (unload_dialog dcl_id)

  (cond
    ((= retour 99)
     (c:DeplacerSolides3D))

    ((and (= retour 1) prefixe_flag)
     (if (and selection (/= selection "") (wcmatch selection "*[0-9]*"))
       (progn
         (setq chaine (strcat "(" selection ")"))
         (setq tryread (vl-catch-all-apply 'read (list chaine)))
         (if (or (vl-catch-all-error-p tryread)
                 (not (listp tryread))
                 (not (vl-every 'numberp tryread))
                 (vl-some '(lambda (i) (not (and (numberp i) (<= 0 i) (< i (length layers))))) tryread))
           (prompt "\n?? Sélection invalide ou hors plage.")
           (progn
             (setq selIndexes tryread)
             (setq selectedLayers (mapcar '(lambda (i) (nth i layers)) selIndexes))
             (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
             (setq layerCol (vla-get-Layers doc))
             (setq count 0 skipped 0)
             (foreach lay selectedLayers
               (if (wcmatch lay (strcat prefix "*"))
                 (setq skipped (1+ skipped))
                 (progn
                   (setq newname (strcat prefix lay))
                   (if (tblsearch "LAYER" newname)
                     (setq skipped (1+ skipped))
                     (progn
                       (vl-catch-all-apply
                         '(lambda ()
                            (vla-put-Name (vla-Item layerCol lay) newname)))
                       (setq count (1+ count)))))))
             (prompt (strcat
               "\n? " (itoa count) " renommé(s), "
               (itoa skipped) " ignoré(s).\n")))))
       (prompt "\n?? Aucune sélection valide.")))

    ((= retour 0)
     (prompt "\nOpération annulée.")))

  (command "._REGEN")
  (princ)
)
