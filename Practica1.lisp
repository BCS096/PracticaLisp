;-----------------------------------------
; Pràctica LISP 
; 21721 - Llenguatges de Programació. Grup 1 (Mallorca)
; Bartomeu Capó Salas
; Damián Gebhard Galeote
; Marc Torres Torres
; Emanuel Hegedüs
;-----------------------------------------
;-----------------------------------------
; PART 1
;-----------------------------------------
;realitza la transposta d'una llista
(defun transposta (l)
    (cond ((null (car l)) nil) 
            (t (cons (mapcar 'car l) (transposta (mapcar 'cdr l)))))
)

;multipplica cada element de la llista
(defun mult (l)
    (cond ((null l) 1)
        (t (*(car l) (mult (cdr l)))))
)

;realitza el producte escalar 
(defun ProducteEscalar (l) 
    (apply '+ (mapcar 'mult (transposta l)))
)


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

;multiplica dos matrius (llistes)
(defun multMatriu (l1 l2)
    (operacio l1 (transposta l2))
)

;multiplica un punt per una matriu
(defun multPunt (punt matriu)
    (unafila punt (transposta matriu))
)

;conversor de graus->radians
(defun grausARadians (graus)
    (/ (* graus pi) 180)
)

;fica un elements al dinal d'una llista
(defun snoc (x l) 
    (cond 
        ((null l) (cons x l))
        (t (cons (car l) (snoc x (cdr l))))
    )
)

;translacio mitjançant 3 punts que després emprarem per a una figura
(defun translacio (dx dy dz) 
    (list '(1 0 0 0) '(0 1 0 0) '(0 0 1 0) (snoc 1 (append (cons dx nil) (cons dy nil) (cons dz nil))))
)

;escalat mitjançant 3 punts que després emprarem per a una figura
(defun escalat (ex ey ez)
    (list (append (cons ex nil) (cons 0 nil) (cons 0 nil) (cons 0 nil))
          (append (cons 0 nil) (cons ey nil) (cons 0 nil) (cons 0 nil))
          (append (cons 0 nil) (cons 0 nil) (cons ez nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

;rotar l'eix X  mitjançant un punt que després emprarem per a una figura
(defun rotax (a)
    (list '(1 0 0 0)
          (append (cons 0 nil) (cons (cos (grausARadians a)) nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil))
          (append (cons 0 nil) (cons (sin (grausARadians a)) nil) (cons (cos (grausARadians a)) nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

;rotar l'eix Y  mitjançant un punt que després emprarem per a una figura
(defun rotay (a)
    (list (append (cons (cos (grausARadians a)) nil) (cons 0 nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil))
          '(0 1 0 0)
          (append (cons (sin (grausARadians a)) nil) (cons 0 nil) (cons (cos (grausARadians a)) nil) (cons 0 nil))
          '(0 0 0 1)      
    )
)

;rotar l'eix Z  mitjançant un punt que després emprarem per a una figura
(defun rotaz (a)
    (list (append (cons (cos (grausARadians a)) nil) (cons (- 0 (sin (grausARadians a))) nil) (cons 0 nil) (cons 0 nil))
          (append (cons (sin (grausARadians a)) nil) (cons (cos (grausARadians a)) nil) (cons 0 nil) (cons 0 nil))
          '(0 0 1 0)
          '(0 0 0 1)      
    )
)

;Inicialització del patró del cub
(defun initCub ()
    (putprop 'cub '((-0.5 -0.5 -0.5) (0.5 -0.5 -0.5) (0.5 -0.5 0.5) (-0.5 -0.5 0.5) (-0.5 0.5 -0.5)
    (0.5 0.5 -0.5) (0.5 0.5 0.5) (-0.5 0.5 0.5)) 'punts)
    (putprop 'cub '((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7) 
    (4 8) (5 6) (6 7) (7 8) (8 5)) 'arestes)
    (putprop 'cub '((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) 
    (4 8 12 5) (9 10 11 12)) 'cares)
)

;Inicialització del patró del prisma
(defun initPrisma ()
    (putprop 'prisma '((-0.5 -0.5 0.5) (0.5 -0.5 0.5) (0 -0.5 -0.5) (-0.5 0.5 0.5) 
    (0.5 0.5 0.5) (0 0.5 -0.5)) 'punts)
    (putprop 'prisma '((1 2) (2 3) (3 1) (1 4) (2 5) (3 6) 
    (4 5) (5 6) (6 4)) 'arestes)
    (putprop 'prisma '((1 2 3) (1 5 7 4) (2 5 8 6)
    (3 6 9 4) (7 8 9)) 'cares)
)

;Inicialització del patró del octaedre
(defun initOctaedre ()
    (putprop 'octaedre '((-0.5 0 -0.5) (0.5 0 -0.5) (0.5 0 0.5) (-0.5 0 0.5) 
    (0 0.5 0) (0 -0.5 0)) 'punts)
    (putprop 'octaedre '((1 2) (2 3) (3 4) (4 1) (1 5) (2 5)
    (3 5) (4 5) (1 6) (2 6) (3 6) (4 6)) 'arestes)
    (putprop 'octaedre '((1 2 3 4) (1 5 6) (2 6 7) (3 7 8) 
    (4 8 5) (1 9 10) (2 10 11) (3 11 12) (4 12 9)) 'cares)
)

;inicialització de l'escena que contindrà la llista de figures
(defun initEscena ()
    (putprop 'escena nil 'figures)          ;inicialmente vacia
)

;inicialitzam el patrons per tenir-los carregats
(defun inicia-patrons ()
    (initCub)
    (initPrisma)
    (initOctaedre)
)

;borrar un element X d'una llista
(defun borra-element (x llista)
    (cond ((null llista) nil)
        ((equal x (car llista)) (cdr llista))
        (t (cons (car llista) (borra-element x (cdr llista))))
    )
)

;borra la figura demanada de la llista de figures, i imprimeix una altre vegada les
;figures però sense la borrada prèviament
(defun borra-figura (f)
    (putprop 'escena (borra-element f (get 'escena 'figures)) 'figures)
    (cls)
    (pinta-figures)
)

;borra de la pantalla la figura demanada pero NO de la llsita de figures, sols de la pantalla
(defun cls-figura (f)  
    (cls)
    (pinta-llista-figures (borra-element f (get 'escena 'figures)))
)

;borra totes les figures de la pantalla y de la llista de figures
(defun borra-figures ()
    (putprop 'escena nil 'figures)
    (cls)
)

;patró de la matriu indentitat
(defun  matriuIdentitat () 
    (list '(1 0 0 0) '(0 1 0 0) '(0 0 1 0) '(0 0 0 1))
)

;ficar una figura a una llista
(defun ficarFigura (x l)
    (snoc x l)
)

;cream una figura dels 3 patrons que hi ha amb un nom y un color. També necessita tenir
;la matriu identitat per defecte
(defun crea-figura (n p c)
    (putprop n p 'patro)
    (putprop n c 'color)
    (putprop n (matriuIdentitat) 'matriu)
    (putprop 'escena (ficarFigura n (get 'escena 'figures)) 'figures)
)

;posa la figura f a la seva posició inicial (matriu identitat a la transformació)
(defun inicia-figura (f)
    (putprop f (matriuIdentitat) 'matriu)
)

;mitjançant la multiplicació de matrius traslladam una figura
(defun trasllada-figura (f x y z)
    (putprop f (multmatriu (get f 'matriu) (translacio x y z)) 'matriu)
)

;mitjançant la multiplicació de matrius rotam X/Y/Z una figura
(defun rota-figura (f x y z)
    (putprop f (multmatriu (get f 'matriu) (rotax x)) 'matriu)
    (putprop f (multmatriu (get f 'matriu) (rotay y)) 'matriu)
    (putprop f (multmatriu (get f 'matriu) (rotaz z)) 'matriu)
)

;mitjançant la multiplicació de matrius escalam una figura
(defun escala-figura (f x y z)
    (putprop f (multmatriu (get f 'matriu) (escalat x y z)) 'matriu)
)

;pinta dos punts d'una figura especifica
(defun pinta-punts (a b f)
    (color (car (get f 'color)) (cadr (get f 'color)) (caddr (get f 'color)) )
    (move (+ (realpart (round (car a))) 320) (+ (realpart (round (car (cdr a)))) 187));lapiz en 1r punto
    (draw (+ (realpart (round (car b))) 320) (+ (realpart (round (car (cdr b)))) 187));desplazamos lapiz al 2r punto pintando
)

;pinta una aresta d'una figura especifica
(defun pinta-aresta-indv (a f) ;(1 2) , figura
    (pinta-punts 
        (multpunt (snoc 1 (NTH (- (car a) 1) (get (get f 'patro) 'punts ) )) (get f 'matriu))
        (multpunt (snoc 1 (NTH (- (car (cdr a)) 1) (get (get f 'patro) 'punts) )) (get f 'matriu))
         f
    )
)

;pinta arestes d'una figura especifica
(defun pinta-arestas (a f) ;1 , figura
    (pinta-aresta-indv
        ;pos -1- de "arestes ((1 2) (2 3) (3 4) (4 1) (1 5) (2 6) (3 7) (4 8) (5 6) (6 7) (7 8) (8 5))" luego (1 2)
        (NTH (- a 1) (get (get f 'patro) 'arestes))
        f
    )
   ;concatena el resultado de llamar una vez a get-punts con car de llista y otra vez a get-punts con cdr de llista luego con eso llama a pinta
)

;pinta una cara individual d'una figura
(defun pinta-cara-indv (c f) ;(1 2 3 4), figura
    (cond ((null c) nil)
        (t (pinta-arestas (car c) f)  (pinta-cara-indv (cdr c) f) ;(1 -2- 3 4) 
        )
    )
)

;pinta les cares d'una figura
(defun pinta-caras (c f) ;((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) (4 8 12 5) (9 10 11 12)) , figura
    (cond ((null c) nil)
        (t (pinta-cara-indv (car c) f) ;(1 2 3 4)
           (pinta-caras (cdr c) f) ;... siguiente cara (x1,x2,x3,x  
        )
    )
)

;pinta una figura demanada
(defun pinta-figura (f) ;figura
    (pinta-caras 
            (get (get f 'patro) 'cares) f ;cares ((1 2 3 4) (1 6 9 5) (2 6 10 7) (3 7 11 8) (4 8 12 5) (9 10 11 12))
    )
)


;recorre lista x llamanda a pinta-figura para cada elem
(defun pinta-llista-figures (x)
    ;mientras no sea nula
    (cond
    ((null x) nil)
        ;pinta figura i
     (t (pinta-figura (car x)) (pinta-llista-figures (cdr x)))  ; ('cub1 'prisma1 'cub2)
    )
)

;NTH xd
(defun agafa-element (pos llista)
    (cond ((null llista) nil)
          ((= pos 0) (car llista))
          (t (agafa-element (- pos 1) (cdr llista) ))
    )
)

;Metodo que extrae lista de figuras de escena
(defun pinta-figures ()
    (pinta-llista-figures (get 'escena 'figures))
)
;-----------------------------------------
; PART 2
;-----------------------------------------
;animació de la rotació d'una figura
(defun anima-rotacio (f)
    (cls)
    (print 'rotacio)
    (pinta-figura f)
    ;segons la tecla que pitjem pasarà diferents esdeveniments
    (setq key (get-key))
    (cond 
        ((= key 336) (rota-figura f -5 0 0)(anima-rotacio f)) ;esquerra
        ((= key 328) (rota-figura f 5 0 0)(anima-rotacio f)) ;dreta
        ((= key 333) (rota-figura f 0 5 0)(anima-rotacio f)) ;adalt
        ((= key 331) (rota-figura f 0 -5 0)(anima-rotacio f)) ;abaix
        ((= key 113)) ;q (menú)
    )
)

;animació de la translacio d'una figura
(defun anima-translacio (f)
    (cls)
    (print 'translacio)
    (pinta-figura f)
    ;segons la tecla que pitjem pasarà diferents esdeveniments
    (setq key (get-key))
    (cond 
        ((= key 331) (trasllada-figura f -5 0 0)(anima-translacio f)) ;esquerra
        ((= key 333) (trasllada-figura f 5 0 0)(anima-translacio f)) ;dreta
        ((= key 328) (trasllada-figura f 0 5 0)(anima-translacio f)) ;adalt
        ((= key 336) (trasllada-figura f 0 -5 0)(anima-translacio f)) ;abaix
        ((= key 113)) ;q (menú)
    )
)

;animació de l'escalat d'una figura
(defun anima-escalat (f)
    (cls)
    (print 'escalat)
    (pinta-figura f)
    ;segons la tecla que pitjem pasarà diferents esdeveniments
    (setq key (get-key))
    (cond 
        ((= key 331) (escala-figura f (/ 1 2) 1 1)(anima-escalat f)) ;esquerra
        ((= key 333) (escala-figura f 2 1 1)(anima-escalat f)) ;dreta
        ((= key 328) (escala-figura f 1 2 1)(anima-escalat f)) ;adalt
        ((= key 336) (escala-figura f 1 (/ 1 2) 1)(anima-escalat f)) ;abaix
        ((= key 113)) ;q (menú)
    )
)

;animació total d'una figura
(defun animacio (f)
    (cls)
    (print 'animacio)
    (pinta-figura f)
    ;segons la tecla que pitjem pasarà diferents esdeveniments
    (setq key (get-key))
    (cond 
        ((= key 114) (anima-rotacio f )(animacio f)) ;r (activa l'animacio de la rotació)
        ((= key 116) (anima-translacio f )(animacio f)) ;t (activa l'animacio de la translació)
        ((= key 101) (anima-escalat f )(animacio f)) ;e (activa l'animacio de l'escalat)
        ((= key 113)) ;q (menu)
    )
    (cls)
)
;-----------------------------------------
;ejecuciones
(initEscena)
(inicia-patrons)
(crea-figura 'o 'octaedre '(255 0 0))
(escala-figura 'o 100 200 100)
(rota-figura 'o 0 30 0)
(crea-figura 'cub1 'cub '(0 0 255))
(escala-figura 'cub1 100 100 100)
(cls)

