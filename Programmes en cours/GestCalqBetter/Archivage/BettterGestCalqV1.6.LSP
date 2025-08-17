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

;=========================================================
;=Version Fonctionelle de Test :			 =
;=Autocad MAP2024 - 2025				 =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

;Début du programme
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
(defun c:CalquesManagerPro ( / dcl_id retour selection prefix chaine tryread selIndexes selectedLayers doc layerCol count skipped newname lay ss edata)

  (vl-load-com)

  ;; DCL
  (setq dcl_file "calques.dcl")
  (if (not (findfile dcl_file))
    (progn (alert (strcat "Fichier DCL manquant : " dcl_file)) (exit)))
  (setq dcl_id (load_dialog dcl_file))
  (if (not (new_dialog "calques_dialog" dcl_id)) (exit))

  ;; Liste des calques
  (setq allLayers '())
  (setq lay (tblnext "LAYER" T))
  (while lay
    (setq allLayers (cons (cdr (assoc 2 lay)) allLayers))
    (setq lay (tblnext "LAYER")))
  (setq layers (vl-sort allLayers '<))

  ;; Affichage
  (start_list "layers_list")
  (mapcar 'add_list layers)
  (end_list)

  ;; Actions
  (action_tile "layers_list" "(setq selection $value)")
  (action_tile "btn_prefixe"
    "(setq prefix (get_tile \"prefixe\"))
     (done_dialog 1)")
;-------------------BTN Filter par nom---------------------
  (action_tile "btn_filtrer_nom"
    "(setq filtre (get_tile \"filtre_nom\"))
     (start_list \"layers_list\" 3)
     (mapcar 'add_list
       (vl-remove-if-not
         '(lambda (n)
            (wcmatch (strcase n) (strcat \"*\" (strcase filtre) \"*\")))
         layers))
     (end_list))")
;-------------------BTN Filter par couleur---------------------
  (action_tile "btn_filtrer_couleur"
    "(setq couleur (atoi (get_tile \"filtre_couleur\")))
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
     (setq layerCol (vla-get-Layers doc))
     (setq filtered '())
     (vlax-for l layerCol
       (if (= (vla-get-ColorIndex l) couleur)
         (setq filtered (cons (vla-get-Name l) filtered))))
     (start_list \"layers_list\" 3)
     (mapcar 'add_list filtered)
     (end_list))")
;-------------------BTN Supprimer les calques vides---------------------
  (action_tile "btn_suppr_vides"
    "(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
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
               (setq deleted (1+ deleted))))))
     (alert (strcat (itoa deleted) \" calque(s) vide(s) supprimé(s).\")))")
;---------------BTN déplacer SOLIDES--------------------
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
;-------------BTN restaurer--------------------------
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
;--------------BTN Renommer--------------------------
  (action_tile "btn_renommer"
  "(progn
     (setq selection $value)
     (setq prefix (get_tile \"prefixe\"))
     (done_dialog 1))")
;-------------BTN Reset------------------------------
  (action_tile "btn_reset"
    "(start_list \"layers_list\" 3)
     (mapcar 'add_list layers)
     (end_list)
     (set_tile \"filtre_nom\" \"\")
     (set_tile \"filtre_couleur\" \"\")")

  (action_tile "cancel" "(done_dialog)")

  ;;  Lancement dialogue
  (setq retour (start_dialog))
  (unload_dialog dcl_id)

  ;;  Post-traitement du renommage
  (if (and (= retour 1) selection (/= selection "") (wcmatch selection "*[0-9]*"))
    (progn
      (setq chaine (strcat "(" selection ")"))
      (setq tryread (vl-catch-all-apply 'read (list chaine)))
      (if (and (listp tryread)
               (vl-every 'numberp tryread)
               (vl-every '(lambda (i) (and (<= 0 i) (< i (length layers)))) tryread))
        (progn
          (setq selIndexes tryread)
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
          (prompt (strcat \"\\n \" (itoa count) \" renommé(s), \" (itoa skipped) \" ignoré(s).")))
        (prompt \"\\n Indices invalides.")))
    (prompt \"\\n Aucune sélection valide."))

  (command "._REGEN")
  (princ)
)