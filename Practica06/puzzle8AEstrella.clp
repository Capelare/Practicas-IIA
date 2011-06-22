#**********************#
;      IIA  10/11      ;
;      Practica 6      ;
; Miguel Gomez Gonzalo ;
#**********************#

(deffacts operadores
   (op arr) (op aba) (op izq) (op der))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones auxiliares ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(deffunction posicion (?num $?elems)
	(bind ?p (member$ ?num $?elems))
	(switch ?p
		(case 1 then (create$ 1 1))
		(case 2 then (create$ 1 2))
		(case 3 then (create$ 1 3))
		(case 4 then (create$ 2 1))
		(case 5 then (create$ 2 2))
		(case 6 then (create$ 2 3))
		(case 7 then (create$ 3 1))
		(case 8 then (create$ 3 2))
		(case 9 then (create$ 3 3))
		(default none)		
	)
)

(deffunction elemento (?x ?y $?elems)
	(nth (+ (* 3 (- ?x 1)) ?y) $?elems)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funciones  algoritmo ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction aplicablep (?op $?coors)
	(bind $?pos-hueco (posicion 0 $?coors))
	(bind ?x (nth 1 $?pos-hueco))
	(bind ?y (nth 2 $?pos-hueco))
	(switch ?op
		(case arr then (> ?x 1))
		(case aba then (< ?x 3))
		(case izq then (> ?y 1))
		(case der then (< ?y 3))
		(default (< 1 0))
	)
)

(deffunction aplicar (?op $?elems)
	(bind $?pos-hueco (posicion 0 $?elems))
	(bind ?x (nth 1 $?pos-hueco))
	(bind ?y (nth 2 $?pos-hueco))
	(switch ?op
		(case arr then
			(bind ?e-a (elemento (- ?x 1) ?y $?elems))	; Elemento que voy a poner en el hueco (arriba)
			(bind ?e-b 0)								; Hueco (abajo)
			(bind ?mov vertical)
		)
		(case aba then
			(bind ?e-b (elemento (+ ?x 1) ?y $?elems))	; Elemento que voy a poner en el hueco (abajo)
			(bind ?e-a 0)								; Hueco (arriba)
			(bind ?mov vertical)
		)
		(case izq then
			(bind ?e-a (elemento ?x (- ?y 1) $?elems))	; Elemento que voy a poner en el hueco (izquierda)
			(bind ?e-b 0)								; Hueco (derecha)
			(bind ?mov horizontal)
		)
		(case der then
			(bind ?e-b (elemento ?x (+ ?y 1) $?elems))	; Elemento que voy a poner en el hueco (derecha)
			(bind ?e-a 0)								; Hueco (izquierda)
			(bind ?mov horizontal)
		)
		(default none)
	)
	
	(switch ?mov
		(case vertical then
			(bind ?a (member$ ?e-a $?elems))			; Posicion en la lista del elemento de arriba
			(bind ?b (+ ?a 3))							; Posicion en la lista del elemento de abajo

			(if (= ?a 1) then							; No sirve la versión general porque se sale de rango (1-1)
				(bind $?retorno (create$ ?e-b (subseq$ $?elems 2 3) ?e-a (subseq$ $?elems 5 9)))
			else
				(if (= ?b 9) then						; No sirve la versión general porque se sale de rango (9+1)
					(bind $?retorno (create$ (subseq$ $?elems 1 5) ?e-b (subseq$ $?elems 7 8) ?e-a))
				else									; Versión general del movimiento vertical
					(bind $?retorno (create$ (subseq$ $?elems 1 (- ?a 1)) ?e-b (subseq$ $?elems (+ ?a 1) (- ?b 1)) ?e-a (subseq$ $?elems (+ ?b 1) 9)))
				)
			)
		)
		(case horizontal then
			(bind ?a (member$ ?e-a $?elems))			; Posicion en la lista del elemento de la izquierda
			(bind ?b (+ ?a 1))							; Posicion en la lista del elemento de la derecha
			
			(if (= ?a 1) then							; No sirve la versión general porque se sale de rango (1-1)
				(bind $?retorno (create$ ?e-b ?e-a (subseq$ $?elems 3 9)))
			else
				(if (= ?b 9) then						; No sirve la versión general porque se sale de rango (9+1)
					(bind $?retorno (create$ (subseq$ $?elems 1 7)) ?e-b ?e-a)
				else									; Versión general del movimiento horizontal
					(bind $?retorno (create$ (subseq$ $?elems 1 (- ?a 1)) ?e-b ?e-a (subseq$ $?elems (+ ?b 1) 9)))
				)
			)
		)
		(default none)
	)

	(return (create$ $?retorno 1)) ;coste 1
)


(deffunction finalp ($?coors)
	(eq $?coors (create$ 1 2 3 8 0 4 7 6 5))
)
  
(deffunction heuristico ($?elems)
	(bind $?pos0 (posicion 0 $?elems))
	(bind $?pos1 (posicion 1 $?elems))
	(bind $?pos2 (posicion 2 $?elems))
	(bind $?pos3 (posicion 3 $?elems))
	(bind $?pos4 (posicion 4 $?elems))
	(bind $?pos5 (posicion 5 $?elems))
	(bind $?pos6 (posicion 6 $?elems))
	(bind $?pos7 (posicion 7 $?elems))
	(bind $?pos8 (posicion 8 $?elems))
	
	(bind ?h1 (+ (abs (- (nth 1 $?pos1) 1)) (abs (- (nth 2 $?pos1) 1))))	; (1,1) 1
	(bind ?h2 (+ (abs (- (nth 1 $?pos2) 1)) (abs (- (nth 2 $?pos2) 2))))	; (1,2) 2
	(bind ?h3 (+ (abs (- (nth 1 $?pos3) 1)) (abs (- (nth 2 $?pos3) 3))))	; (1,2) 3
	(bind ?h8 (+ (abs (- (nth 1 $?pos8) 2)) (abs (- (nth 2 $?pos8) 1))))	; (2,1) 8
	(bind ?h0 (+ (abs (- (nth 1 $?pos0) 2)) (abs (- (nth 2 $?pos0) 2))))	; (2,2) 0
	(bind ?h4 (+ (abs (- (nth 1 $?pos4) 2)) (abs (- (nth 2 $?pos4) 3))))	; (2,3) 4
	(bind ?h7 (+ (abs (- (nth 1 $?pos7) 3)) (abs (- (nth 2 $?pos7) 1))))	; (3,1) 7
	(bind ?h6 (+ (abs (- (nth 1 $?pos6) 3)) (abs (- (nth 2 $?pos6) 2))))	; (3,2) 6
	(bind ?h5 (+ (abs (- (nth 1 $?pos5) 3)) (abs (- (nth 2 $?pos5) 3))))	; (3,3) 5
	
	(return (+ ?h0 ?h1 ?h2 ?h3 ?h4 ?h5 ?h6 ?h7 ?h8))
)

(deffacts inicial
   (e 8 5 6 3 0 1 4 2 7)
)