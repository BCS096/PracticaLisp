(defun transposta (l)
    (cond ((null (car l)) nil) 
            (t (cons (mapcar 'car l) (transposta (mapcar 'cdr l)))))
)

(defun ProducteEscalar (l)
    (apply '+ (mapcar 'mult (transposta l))))

(defun unaFila (fila filas)
    (cond
        ((null filas) nil)
        (t (cons (producteEscalar (list fila (car filas) ) ) 
        (unaFila fila (cdr filas)) ) )
    )
)

(defun operacio (l1 l2)
    (cond 
        ((equal (cdr l1) nil) (cons (unaFila (car l1) l2) nil))
        (t (cons (unaFila (car l1) l2) (operacio (cdr l1) l2)   ))
    )
)

(defun multMatriu (l1 l2)
    (operacio l1 (transposta l2))
)

;(multMatriu '((1 2 3 2) (2 3 4 5) (1 5 6 3) (1 2 2 2)) 
;'((2 3 5 7) (2 3 1 3) (3 2 2 1) (3 4 5 2)))

;----------------------------------------------------------------------------------------------------------------------------------------------

(defun grausARadians (graus)
    (/ (* graus pi) 180)
)

(defun snoc (x l) 
    (cond 
        ((null l) (cons x l))
        (t (cons (car l) (snoc x (cdr l))))
    )
)

(defun translacio (dx dy dz) 
    (list '(1 0 0 0) '(0 1 0 0) '(0 0 1 0) (snoc 1 (append (cons dx nil) (cons dy nil) (cons dz nil))))
)

(defun escalat (ex ey ez)
    (list (append (cons ex nil) (cons 0 nil) (cons 0 nil) (cons 0 nil))
          (append (cons 0 nil) (cons ey nil) (cons 0 nil) (cons 0 nil))
          (append (cons 0 nil) (cons 0 nil) (cons ez nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

(defun rotax (a)
    (list '(1 0 0 0)
          (append (cons 0 nil) (cons (cos (grausARadians a)) nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil))
          (append (cons 0 nil) (cons (sin (grausARadians a)) nil) (cons (cos (grausARadians a)) nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

(defun rotay (a)
    (list (append (cons (cos (grausARadians a)) nil) (cons 0 nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil))
          '(0 1 0 0)
          (append (cons (sin (grausARadians a)) nil) (cons 0 nil) (cons (cos (grausARadians a)) nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

(defun rotaz (a)
    (list (append (cons (cos (grausARadians a)) nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil) (cons 0 nil))
          (append (cons (sin (grausARadians a)) nil) (cons (cos (grausARadians a)) nil) (cons 0 nil) (cons 0 nil))
          '(0 0 1 0)
          '(0 0 0 1)      
    )
)