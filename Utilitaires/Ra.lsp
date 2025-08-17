; Auteur : Patrick_35©
; Head :  Raccord
; Comm : Fait un raccord sans joindre

;=========================================================
;=Version Fonctionelle :				 	=
;=Autocad MAP2024 - 2025					=
;=Merci de regarder la Doc avant utilisation du programme		=
;=========================================================


(defun c:raccordsjoindre(/ code1 code2 ent1 ent2 pt1 pt2 pti)

  (defun err (msg)
    (if (/= msg "Fonction annulée")
      (princ (strcat "\nErreur : " msg))
      (princ msg)
    )
    (if ent1
      (redraw (cdr (assoc -1 ent1)) 4)
    )
    (setq *error* olderr)
    (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
    (princ)
  ) 

  (defun selection(ent pts / code lst pt)
    (cond
      ((eq (cdr (assoc 0 ent)) "LINE")
        (if (< (distance (trans pts 1 0) (cdr (assoc 10 ent)))
               (distance (trans pts 1 0) (cdr (assoc 11 ent))))
          (setq code 10)
          (setq code 11)
        )
      )
      ((eq (cdr (assoc 0 ent)) "LWPOLYLINE")
        (setq lst (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) ent)))
        (if (< (distance (trans pts 1 0) (last lst)) (distance (trans pts 1 0) (car lst)))
          (setq code (list (last lst) (cadr (reverse lst))))
          (setq code (list (car lst) (cadr lst)))
        )
      )
    )
    code
  )

  (vl-load-com)
  (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
  (setq olderr *error* *error* err)
  (if (setq ent1 (car (entsel "\nSélectionner la première ligne/polyligne : ")))
    (progn
      (setq ent1 (entget ent1))
      (if (setq code1 (selection ent1 (cadr (grread T))))
        (progn
          (redraw (cdr (assoc -1 ent1)) 3)
          (if (setq ent2 (car (entsel "\nSélectionner la seconde ligne/polyligne : ")))
            (progn
              (setq ent2 (entget ent2))
              (if (setq code2 (selection ent2 (cadr (grread T))))
                (progn
                  (redraw (cdr (assoc -1 ent1)) 4)
                  (if (eq (type code1) 'INT)
                    (setq pt1 (list (cdr (assoc 10 ent1)) (cdr (assoc 11 ent1))))
                    (setq pt1 code1)
                  )
                  (if (eq (type code2) 'INT)
                    (setq pt2 (list (cdr (assoc 10 ent2)) (cdr (assoc 11 ent2))))
                    (setq pt2 code2)
                  )
                  (setq pti (trans (inters (car pt1) (cadr pt1) (car pt2) (cadr pt2) nil) 0 1))
                  (if (eq (type code1) 'INT)
                    (setq ent1 (subst (cons code1 pti) (assoc code1 ent1)    ent1))
                    (setq ent1 (subst (cons 10 pti)    (cons 10 (car code1)) ent1))
                  )
                  (entmod ent1)
                  (if (eq (type code2) 'INT)
                    (setq ent2 (subst (cons code2 pti) (assoc code2 ent2)    ent2))
                    (setq ent2 (subst (cons 10 pti)    (cons 10 (car code2)) ent2))
                  )
                  (entmod ent2)
                )
              )
            )
            (redraw (cdr (assoc -1 ent1)) 4)
          )
        )
      )
    )
  )
  (setq *error* olderr)
  (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
  (princ)
)

(princ "\nRA.LSP chargé. Tapez RA pour l'exécuter")
(princ)