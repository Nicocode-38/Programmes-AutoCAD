; Auteur : Nicolas PISOT
; Head : Renommer des calques Efficacement pour filtrage
; Comm : Permet de rennomer des calques efficacement depuis une interface DCL

;=========================================================
;=Version Fonctionelle :				 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================
(defun c:CalquesUI ()
  (vl-load-com)
  (setq dcl_file "calques.dcl")

  (if (not (findfile dcl_file))
    (progn
      (alert (strcat "Fichier DCL manquant : " dcl_file))
      (exit)))

  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "calques_dialog" dcl_id))
    (exit))

  ;; Collecte des calques
  (setq allLayers '())
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq allLayers (cons (cdr (assoc 2 lay)) allLayers))
    (setq lay (tblnext "LAYER")))
  (setq layers (vl-sort allLayers '<))
  (setq selection "" prefix "" prefixe_flag nil)

  ;; Affichage initial
  (start_list "layers_list")
  (mapcar 'add_list layers)
  (end_list)

  ;; Actions DCL
  (action_tile "layers_list" "(setq selection $value)")
  (action_tile "btn_prefixe"
    "(progn
       (setq prefix (get_tile \"prefixe\"))
       (setq prefixe_flag T)
       (done_dialog 1))")

  (action_tile "btn_filtrer"
    "(progn
       (setq couleurStr (get_tile \"filtre_couleur\"))
       (if (and couleurStr (/= couleurStr \"\") (> (atoi couleurStr) 0) (<= (atoi couleurStr) 255))
         (progn
           (setq couleurACI (atoi couleurStr))
           (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
           (setq layerCol (vla-get-Layers doc))
           (setq current (getvar \"CLAYER\"))
           (vlax-for lay layerCol
             (vl-catch-all-apply
               '(lambda ()
                  (setq lname (vla-get-Name lay))
                  (setq layColorIndex (vla-get-ColorIndex (vla-get-TrueColor lay)))
                  (if (= layColorIndex couleurACI)
                    (vla-put-Freeze lay :vlax-false)
                    (if (and (/= lname current) (/= lname \"0\") (vlax-write-enabled-p lay))
                      (vla-put-Freeze lay :vlax-true))))))

           (prompt (strcat \"\\nLes calques de couleur ACI \" (itoa couleurACI) \" sont visibles, les autres sont gelés.\\n\"))
           (command \"._REGEN\"))
         (alert \"Veuillez entrer une valeur ACI valide entre 1 et 255.\")))")

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
       (prompt \"\\n? Tous les calques ont été dégélés.\")
       (command \"._REGEN\"))")

  ;; Lancement
  (setq retour (start_dialog))
  (unload_dialog dcl_id)

  ;; Traitement post-dialogue
  (cond
    ((and (= retour 1) prefixe_flag)
     (if (and selection (/= selection "") (wcmatch selection "*[0-9]*"))
       (progn
         (setq chaine (strcat "(" selection ")"))
         (setq tryread (vl-catch-all-apply 'read (list chaine)))
         (if (or (vl-catch-all-error-p tryread) (not (listp tryread)))
           (prompt "\n?? Sélection invalide ou liste incorrecte.")
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
               "\n? " (itoa count) " calque(s) renommé(s), "
               (itoa skipped) " ignoré(s).\n")))))
       (prompt "\n?? Aucun calque sélectionné ou liste vide.")))
    ((= retour 0) (prompt "\nOpération annulée.")))

  (princ)
)


