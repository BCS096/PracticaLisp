(defun transposta (l)
    (cond ((null (car l)) nil) 
            (t (cons (mapcar 'car l) (transposta (mapcar 'cdr l)))))
)

(defun mult (l)
    (cond ((null l) 1)
        (t (*(car l) (mult (cdr l)))))
)

(defun ProducteEscalar (l) 
    (apply '+ (mapcar 'mult (transposta l)))
)

; fila -> un punto, filas -> una matriz
(defun unaFila (fila filas)
    (cond
        ((null filas) nil)
        (t (cons (producteEscalar (list fila (car filas))) (unaFila fila (cdr filas))))
    )
)

(defun operacio (l1 l2)
    (cond 
        ((equal (cdr l1) nil) (cons (unaFila (car l1) l2) nil))
        (t (cons (unaFila (car l1) l2) (operacio (cdr l1) l2)))
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

;--------------------------------------------------------------------------------------------------------------------------

;patró cub

;punts ((0 0 0) (1 0 0) (1 0 1) (0 0 1) (0 1 0) (1 1 0) (1 1 1) (0 1 1))
;arestes ((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7) (4 8) (5 6) (6 7) (7 8) (8 5))
;cares ((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) (4 8 12 5) (9 10 11 12))

(defun initCub ()
    (putprop 'cub '((0 0 0) (1 0 0) (1 0 1) (0 0 1) (0 1 0)
    (1 1 0) (1 1 1) (0 1 1)) 'punts)
    (putprop 'cub '((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7) 
    (4 8) (5 6) (6 7) (7 8) (8 5)) 'arestes)
    (putprop 'cub '((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) 
    (4 8 12 5) (9 10 11 12)) 'cares)
)

;patró prisma

;punts ((0 0 0) (1 0 0) (0 0 1) (0 1 0) (1 1 0) (0 1 1))
;arestes ((1 2) (2 3) (3 1) (1 4) (2 5) (3 6) (4 5) (5 6) (6 4))
;cares ((1 2 3) (1 5 7 4) (2 5 8 6) (3 6 9 4) (7 8 9))

(defun initPrisma ()
    (putprop 'prisma '((0 0 0) (1 0 0) (0 0 1) (0 1 0) 
    (1 1 0) (0 1 1)) 'punts)
    (putprop 'prisma '((1 2) (2 3) (3 1) (1 4) (2 5) (3 6) 
    (4 5) (5 6) (6 4)) 'arestes)
    (putprop 'prisma '((1 2 3) (1 5 7 4) (2 5 8 6)
    (3 6 9 4) (7 8 9)) 'cares)
)

;patró octaedre

;punts ((0 0 0) (1 0 0) (1 0 1) (0 0 1) (0.5 1 0.5) (0.5 -1 0.5))
;arestes ((1 2) (2 3) (3 4) (4 1) (1 5) (2 5) (3 5) (4 5) (1 6) (2 6) (3 6) (4 6))
;cares ((1 2 3 4) (1 5 6) (2 6 7) (3 7 8) (4 8 5) (1 9 10) (2 10 11) (3 11 12) (4 12 9))

(defun initOctaedre ()
    (putprop 'octaedre '((0 0 0) (1 0 0) (1 0 1) (0 0 1) 
    (0.5 1 0.5) (0.5 -1 0.5)) 'punts)
    (putprop 'octaedre '((1 2) (2 3) (3 4) (4 1) (1 5) (2 5)
    (3 5) (4 5) (1 6) (2 6) (3 6) (4 6)) 'arestes)
    (putprop 'octaedre '((1 2 3 4) (1 5 6) (2 6 7) (3 7 8) 
    (4 8 5) (1 9 10) (2 10 11) (3 11 12) (4 12 9)) 'cares)
)

(defun initEscena ()
    (putprop 'escena nil 'figures)          ;inicialmente vacia
)

(defun inicia-patrons ()
    (initCub)
    (initPrisma)
    (initOctaedre)
)

;--------------------------------------------------------------------
; Manu
;--------------------------------------------------------------------
(defun borra-element (x llista)
    (cond ((null llista) nil)
        ((equal x (car llista)) (cdr llista))
        (t (cons (car llista) (borra x (cdr llista))))
    )
)

(defun borra-figura (f)
    (putprop 
    'escena 
        (
            borra-element
            (
                ;f -> element
                f
                ;(get 'escena 'figures) -> llista
                (get 'escena 'figures)
            )
        )
    'figures
    )
)

; Método de pintar pero sustituyendo con el color blanco, esperar a que se haga (Marc dale :) )
(defun cls-figura (f)
    ()
)

(defun borra-figures ()
    (putprop 'escena nil 'figures)
    (cls)
)
;--------------------------------------------------------------------
;DAM
;--------------------------------------------------------------------
(defun  matriuIdentitat () 
    (list '(1 0 0 0) '(0 1 0 0) '(0 0 1 0) '(0 0 0 1))
)

(defun ficarFigura (x l)
    (snoc x l)
)

;(crea-figura nom patró color): És una funció que permet la creació d’una figura 3D a partir
;del patró triat i guarda aquesta figura dins la propietat “figures” de tipus llista d’un àtom
;“escena”.
(defun crea-figura (n p c)
    (putprop n p 'patro)
    (putprop n c 'color)
    (putprop n (matriuIdentitat) 'matriu)
    (putprop 'escena (ficarFigura n (get 'escena 'figures)) 'figures)
)

;--------------------------------------------------------------------
;(inicia-figura f): posa la figura f a la seva posició inicial (matriu identitat a la
;transformació)
(defun inicia-figura (f)
    (putprop f (matriuIdentitat) 'matriu)
)

;--------------------------------------------------------------------
;COTI
;--------------------------------------------------------------------

(defun trasllada-figura (f x y z) ;'cub1 2 3 5
    (putprop f (multmatriu (get f 'matriu) (translacio x y z)) 'matriu)
)

(defun rota-figura (f x y z)
    (putprop f (multmatriu (get f 'matriu) (rotax x)) 'matriu)
    (putprop f (multmatriu (get f 'matriu) (rotay y)) 'matriu)
    (putprop f (multmatriu (get f 'matriu) (rotaz z)) 'matriu)
)

(defun escala-figura (f x y z)
    (putprop f (multmatriu (get f 'matriu) (escalat x y z)) 'matriu)
)
;--------------------------------------------------------------------
;MARC
;--------------------------------------------------------------------
;[2:53 p. m., 28/4/2022] Coti: Te falta antes de pintar multiplicar todos los puntos por la matriz
;de transformación de la figura i tmb sumar a cada punto no me acuerdo cuanto para que salga en medio de la pantalla
;color
(defun pinta-punts (a b) ;(0 0 0) , (1 0 0) Se ignora la z (3r parametro)
    (move (car a) (cdr a));lapiz en 1r punto
    (draw (car b) (cdr b));desplazamos lapiz al 2r punto pintando
)

(defun pinta-aresta-indv (a f) ;(1 2) , figura
    pinta-punts( 
        ;pos (-1- 2) de "punts ((0 0 0) (1 0 0) (1 0 1) (0 0 1) (0 1 0) (1 1 0) (1 1 1) (0 1 1))" luego (0 0 0)
        (NTH (car a) (get (get f 'patro) 'punts)) 
        ;pos (1 -2-) de "punts ((0 0 0) (1 0 0) (1 0 1) (0 0 1) (0 1 0) (1 1 0) (1 1 1) (0 1 1))" luego (1 0 0)
        (NTH (cdr a) (get (get f 'patro) 'punts))
    )
)

(defun pinta-arestas (a f) ;1 , figura
    pinta-aresta-indv
    (
        ;pos -1- de "arestes ((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7) (4 8) (5 6) (6 7) (7 8) (8 5))" luego (1 2)
        (NTH (c) (get (get f 'patro) 'arestes))
        f
    )
   ;concatena el resultado de llamar una vez a get-punts con car de lllista y otra vez a get-punts con cdr de llista luego con eso llama a pinta
)
(defun pinta-cara-indv (c f) ;(1 2 3 4), figura
    (cond ((null c) nil)
        (t (pinta-arestas ((car c)f) ;(-1- 2 3 4)
           (pinta-cara-indv ((cdr c)f)) ;(1 -2- 3 4) 
           )
        )
    )
)
(defun pinta-caras (c f) ;((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) (4 8 12 5) (9 10 11 12)) , figura
    (cond ((null c) nil)
        (t (pinta-cara-indv ((car c)f) ;(1 2 3 4)
           (pinta-caras (cdr c) f) ;... siguiente cara (x1,x2,x3,x4)
           )
        )
    )
)
(defun pinta-figura (f) ;figura
    pinta-caras 
        (
            (get (get f 'patro) 'cares) ;cares ((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) (4 8 12 5) (9 10 11 12))
        )
)
;recorre lista x llamanda a pinta-figura para cada elem
(defun pinta-llista-figures (x llista)
    ;mientras no sea nula
    (cond ((null x) nil)
        ;pinta figura i
        (t (pinta-figura (car x)
            ;pasamos figura i++ 
           (pinta-llista-figures (cdr x))
           )
        )
    )
)
;Metodo que extrae lista de figuras de escena
(defun pinta-figures ()
    pinta-llista-figures ;(x llista)
        (
            (get 'escena 'figures)
        )
)


;-----------------------------------------
;ejecuciones
(initEscena)
(inicia-patrons)