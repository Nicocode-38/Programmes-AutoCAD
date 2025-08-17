; ConcaveHULL
;Détermine le contour concave d'un ensemble de points, blocs ou cercles
;Version 1.0 - 2023-10-01
;By Marc Rouleau - https://www.cadxp.com
; Copyright (c) 2023 Marc Rouleau
; Licensed under the MIT License
(defun c:MR-ConcaveHull ( / *error* unique uniquecoord car-sort trimlst numrayintlil cmde ucsf ss ti i p pl loop pp pln f lil pea p1 p2 li )

  (defun *error* ( m )
    (if ucsf
      (progn
        (command-s "_.ucs" "_p")
        (command-s "_.zoom" "_p")
      )
    )
    (if (= 8 (logand 8 (getvar (quote undoctl))))
      (command-s "_.undo" "_e")
    )
    (if pea
      (setvar (quote peditaccept) pea)
    )
    (if cmde
      (setvar (quote cmdecho) cmde)
    )
    (if m
      (prompt m)
    )
    (princ)
  )

  (defun unique ( lst )
    (if lst
      (cons
        (car lst)
        (unique
          (vl-remove-if
            (function (lambda ( x )
              (equal x (car lst) 1e-6)
            ))
            (cdr lst)
          )
        )
      )
    )
  )

  (defun uniquecoord ( lst a )
    (if lst
      (cons
        (car lst)
        (uniquecoord
          (vl-remove-if
            (function (lambda ( x )
              (cond
                ( (= a 0.0) (equal (cadr x) (cadar lst) 1e-6) )
                ( (= a (* 0.5 pi)) (equal (car x) (caar lst) 1e-6) )
              )
            ))
            (cdr lst)
          )
          a
        )
      )
    )
  )

  (defun car-sort ( lst cmp / rtn )
    (setq rtn (car lst))
    (foreach itm (cdr lst)
      (if (apply cmp (list itm rtn))
        (setq rtn itm)
      )
    )
    rtn
  )

  (defun trimlst ( lst n / ll )
    (repeat n
      (setq ll (cons (car lst) ll))
      (setq lst (cdr lst))
    )
    (vl-remove nil (reverse ll))
  )
;|
  (defun numrayintlil ( p lil a / ip lst )
    (foreach li lil
      (if (setq ip (inters p (polar p a 1e+6) (car li) (cadr li)))
        (setq lst (cons ip lst))
      )
    )
    (length (uniquecoord (unique lst) a))
  )
|;
  (defun numrayintlil ( p lil a / ip l1 l2 l3 )
    (foreach li lil
      (if (setq ip (inters p (polar p a 1e+6) (car li) (cadr li)))
        (setq l1 (cons ip l1))
      )
    )
    (setq l1 (unique l1))
    (foreach li lil
      (if (setq ip (inters (polar p (+ a (* 0.5 pi)) 1e-2) (polar (polar p (+ a (* 0.5 pi)) 1e-2) a 1e+6) (car li) (cadr li)))
        (setq l2 (cons ip l2))
      )
    )
    (setq l2 (unique l2))
    (foreach li lil
      (if (setq ip (inters (polar p (- a (* 0.5 pi)) 1e-2) (polar (polar p (- a (* 0.5 pi)) 1e-2) a 1e+6) (car li) (cadr li)))
        (setq l3 (cons ip l3))
      )
    )
    (setq l3 (unique l3))
    (cond
      ( (and
          (= (length l1) (length l2) (length l3))
          (vl-every
            (function (lambda ( x )
              (vl-some
                (function (lambda ( y )
                  (equal x y 0.1)
                ))
                (append l2 l3)
              )
            ))
            l1
          )
        )
        (length l1)
      )
      ( (and
          (> (length l1) (length l2))
          (> (length l1) (length l3))
        )
        (length l2)
      )
      ( t
        (1+ (length (vl-remove-if (function (lambda ( x ) (vl-some (function (lambda ( y ) (equal x y 0.1))) (append l2 l3)))) l1)))
      )
    )
  )

  (setq cmde (getvar (quote cmdecho)))
  (setvar (quote cmdecho) 0)
  (if (= 8 (logand 8 (getvar (quote undoctl))))
    (vl-cmdf "_.undo" "_e")
  )
  (vl-cmdf "_.undo" "_m")
  (if (= 0 (getvar (quote worlducs)))
    (progn
      (vl-cmdf "_.ucs" "_w")
      (vl-cmdf "_.plan" "")
      (setq ucsf t)
    )
  )
  (alert "Select points, blocks or circles... WARNING : SELECTED ENTITIES MUST BELONG ONE POINT CLOUD GROUP - ISLANDS ARE NOT ALLOWED...")
  (if (setq ss (ssget (list (cons 0 "POINT,INSERT,CIRCLE"))))
    (progn
      (setq ti (car (_vl-times)))
      (repeat (setq i (sslength ss))
        (setq p
          (append
            (mapcar (function +)
              (list 0.0 0.0)
              (cdr (assoc 10 (entget (ssname ss (setq i (1- i))))))
            )
            (list 0.0)
          )
        )
        (setq pl (cons p pl))
      )
      (setq p
        (car-sort pl
          (function (lambda ( a b )
            (if (equal (car a) (car b) 1e-6)
              (< (cadr a) (cadr b))
              (< (car a) (car b))
            )
          ))
        )
      )
      (setq loop t)
      (while loop
        (if (null pp)
          (setq pp
            (car-sort
              (trimlst
                (vl-sort (vl-remove p pl)
                  (function (lambda ( a b )
                    (< (distance p a) (distance p b))
                  ))
                )
                10
              )
              (function (lambda ( a b )
                (if
                  (equal
                    (rem (+ (angle p a) (* 0.5 pi)) (* 2 pi))
                    (rem (+ (angle p b) (* 0.5 pi)) (* 2 pi))
                    1e-6
                  )
                  (< (distance p a) (distance p b))
                  (< (rem (+ (angle p a) (* 0.5 pi)) (* 2 pi)) (rem (+ (angle p b) (* 0.5 pi)) (* 2 pi)))
                )
              ))
            )
          )
          (progn
            (vl-cmdf "_.ucs" "_3p" "_non" (caar lil) "_non" (cadar lil) "")
            (vl-cmdf "_.ucs" "_z" -90.0)
            (vl-cmdf "_.ucs" "_m" "_non" (trans (cadar lil) 0 1))
            (setq pln
              (mapcar
                (function (lambda ( x )
                  (trans x 0 1)
                ))
                (vl-remove-if
                  (function (lambda ( y )
                    (vl-some
                      (function (lambda ( z / ip )
                        (and
                          (setq ip (inters p y (car z) (cadr z)))
                          (not (equal ip p 1e-6))
                          (not (equal ip y 1e-6))
                          (not (equal ip (car z) 1e-6))
                          (not (equal ip (cadr z) 1e-6))
                        )
                      ))
                      (vl-remove-if
                        (function (lambda ( q )
                          (or
                            (equal p (car q) 1e-6)
                            (equal p (cadr q) 1e-6)
                          )
                        ))
                        lil
                      )
                    )
                  ))
                  pl
                )
              )
            )
            (setq pln
              (trimlst
                (vl-sort pln
                  (function (lambda ( a b )
                    (< (distance (list 0.0 0.0 0.0) a) (distance (list 0.0 0.0 0.0) b))
                  ))
                )
                10
              )
            )
            (if
              (and
                (> (length lil) 3)
                (vl-some
                  (function (lambda ( x )
                    (equal (car (last lil)) (trans x 1 0) 1e-6)
                  ))
                  pln
                )
              )
              (progn
                (setq pp
                  (car-sort pln
                    (function (lambda ( a b )
                      (if (equal (angle (list 0.0 0.0) a) (angle (list 0.0 0.0) b) 1e-6)
                        (< (distance (list 0.0 0.0 0.0) a) (distance (list 0.0 0.0 0.0) b))
                        (< (angle (list 0.0 0.0) a) (angle (list 0.0 0.0) b))
                      )
                    ))
                  )
                )
                (setq f t)
              )
              (if (null f)
                (setq pp
                  (car-sort pln
                    (function (lambda ( a b )
                      (if (equal (angle (list 0.0 0.0) a) (angle (list 0.0 0.0) b) 1e-6)
                        (< (distance (list 0.0 0.0 0.0) a) (distance (list 0.0 0.0 0.0) b))
                        (< (angle (list 0.0 0.0) a) (angle (list 0.0 0.0) b))
                      )
                    ))
                  )
                )
                (setq pp (trans (car (last lil)) 0 1))
              )
            )
            (setq pp (trans pp 1 0))
            (vl-cmdf "_.ucs" "_p")
            (vl-cmdf "_.ucs" "_p")
            (vl-cmdf "_.ucs" "_p")
          )
        )
        (setq lil (cons (list p pp) lil))
        (setq p pp)
        (setq pl
          (vl-remove-if
            (function (lambda ( x )
              (equal x p 1e-6)
            ))
            pl
          )
        )
        (if (equal p (car (last lil)) 1e-6)
          (setq loop nil)
        )
      )
      (setq pln nil)
      (foreach p pl
        (if
          (not
            (or
              (= (rem (numrayintlil p lil 0.0) 2) 1)
              (= (rem (numrayintlil p lil (* 0.5 pi)) 2) 1)
            )
          )
          (setq pln (cons p pln))
        )
      )
      (foreach p pln
        (setq p1
          (car-sort
            (apply (function append) lil)
            (function (lambda ( a b )
              (< (distance p a) (distance p b))
            ))
          )
        )
        (setq p2
          (car-sort
            (vl-remove-if
              (function (lambda ( x )
                (equal x p1 1e-6)
              ))
              (apply (function append) lil)
            )
            (function (lambda ( a b )
              (< (distance p a) (distance p b))
            ))
          )
        )
        (setq li
          (vl-some
            (function (lambda ( x )
              (if
                (or
                  (and
                    (equal p1 (car x) 1e-6)
                    (equal p2 (cadr x) 1e-6)
                  )
                  (and
                    (equal p1 (cadr x) 1e-6)
                    (equal p2 (car x) 1e-6)
                  )
                )
                x
              )
            ))
            lil
          )
        )
        (setq lil (vl-remove li lil))
        (setq lil (cons (list p1 p) lil))
        (setq lil (cons (list p2 p) lil))
      )
      (setq ss (ssadd))
      (foreach li lil
        (ssadd
          (entmakex
            (list
              (cons 0 "LINE")
              (cons 10 (car li))
              (cons 11 (cadr li))
            )
          )
          ss
        )
      )
      (setq pea (getvar (quote peditaccept)))
      (setvar (quote peditaccept) 1)
      (vl-cmdf "_.pedit" "_m" ss "" "_j")
      (while (< 0 (getvar (quote cmdactive)))
        (vl-cmdf "")
      )
      (prompt "\nElapsed time : ") (prompt (rtos (- (car (_vl-times)) ti) 2 16)) (prompt " milliseconds...")
    )
  )
  (prompt "\nFor UNDO - type \"UNDO\" - \"Back\" option...")
  (*error* nil)
)