; Auteur : Nicolas PISOT
; Head : Controler les surfaces des hachures
; File : ControlHatchSurface.lsp
; Comm : HacthControl
; Vers : 1.0
; Date de mise à jour : 15/09/2025
; Update : Fixpoly ajouté

;=========================================================
;Suivi des versions
;-
;V1 - Première diffusion.
;-


;=========================================================
;=Version Fonctionelle de Test :			                   =
;=Autocad MAP2025 - 2026				                         =
;=Covadis 2026                                           =
;=Merci de regarder la Doc avant utilisation du programme=
;=========================================================

(setq *hachuresProblemes* nil) ; Variable globale pour stocker les hachures défectueuses

(defun c:ControleHachuresSansAire ( / ss i ent obj area)
  (vl-load-com)
  (setq ss (ssget "X" '((0 . "HATCH"))))
  (setq *hachuresProblemes* '()) ; Réinitialise la liste

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq area nil)

        (if (vl-catch-all-error-p
              (setq area (vl-catch-all-apply 'vla-get-area (list obj)))
            )
          (progn
            (princ (strcat "\n⚠️ Hachure à l'index " (itoa i) " : aire non calculable."))
            (vla-put-color obj 2)
            (setq *hachuresProblemes* (cons ent *hachuresProblemes*))
          )
          (if (= area 0.0)
            (progn
              (princ (strcat "\n⚠️ Hachure à l'index " (itoa i) " : aire = 0."))
              (vla-put-color obj 2)
              (setq *hachuresProblemes* (cons ent *hachuresProblemes*))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ "\n✅ Contrôle terminé. Les hachures problématiques sont en jaune.")
    )
    (princ "\n❌ Aucune hachure trouvée.")
  )
  (princ)
)


(defun c:SelectionHachuresDefectueuses ( / ssProblemes)
  (if *hachuresProblemes*
    (progn
      (setq ssProblemes (ssadd))
      (foreach e *hachuresProblemes*
        (ssadd e ssProblemes)
      )
      (sssetfirst nil ssProblemes) ; Active la sélection dans AutoCAD
      (princ "\n✅ Sélection des hachures défectueuses activée. Tu peux maintenant les copier.")
    )
    (princ "\n⚠️ Aucune hachure défectueuse enregistrée. Lance d'abord ControleHachuresSansAire.")
  )
  (princ)
)
