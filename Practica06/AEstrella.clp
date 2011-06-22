
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Implementacion CLIPS del algoritmo de Dijkstra;;;;
;;;;;;;;;;;;;;;;J. L. Pérez de la Cruz;;;;;;;;;;;;;;;;
;;;;;;;;;Modificado por Miguel Gómez Gonzalo;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;mayo 2011;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;EJEMPLO DE ESPACIO DE ESTADOS;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;(deffacts operadores
;	(op arr) (op aba) (op izq) (op der))


;(deffunction aplicablep (?op $?coors)
;	(bind ?x (nth 1 $?coors))
;	(bind ?y (nth 2 $?coors))
;	(switch ?op
;		(case arr then TRUE)
;		(case aba then (> ?y 0))
;		(case izq then (> ?x 0))
; 		(case der then TRUE)
;		(default none)
;	)
;)

;(deffunction aplicar (?op $?coors)
;	(bind ?x (nth 1 $?coors))
;	(bind ?y (nth 2 $?coors))
;	(switch ?op
;		(case arr then (create$ ?x (+ ?y 1) 1))
;		(case aba then (create$ ?x (- ?y 1) 1))
;		(case izq then (create$ (- ?x 1) ?y 1))
;		(case der then (create$ (+ ?x 1) ?y 1))
;		(default none)
;	)
;)


;(deffunction finalp ($?coors)
;	(bind ?x (nth 1 $?coors))
;	(bind ?y (nth 2 $?coors))  
; 	(and (eq ?x 3) (eq ?y 3))
;)
  
;(deffunction heuristico ($?coors)
;	(+ 0 0)
;)

;(deffacts inicial
;	(e 0 0)
;)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;ALGORITMO;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule iniciar
=>
	(assert (fase inicio))
)



(defrule iniciar-busqueda
	?h1 <- (fase inicio)
	?h2 <- (e $?e)
=>
	(retract ?h1)
	(assert (frontera ?h2)
			(g ?h2 start 0)
			(h ?h2 (heuristico $?e))
			(fase extraer)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;SELECCION DE NODO;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule extraer-hay
	?h1 <- (fase extraer)
	?h2 <- (e $?e)
	?h3 <- (frontera ?h2)
	(g ?h2 ? ?c2)
	(h ?h2 ?heur2)
	(not
		(and
			(frontera ?h)
			(h ?h ?heur)
			(g ?h ?p ?c)
			(test (< (+ ?c ?heur) (+ ?c2 ?heur2)))
		)
	)
=>
	(retract ?h1)
	(assert (fase expandir)
			(seleccionado  ?h2)
	)
)

(defrule extraer-no-hay
	?h <- (fase extraer)
	(not (frontera ?))
=>
	(retract ?h)
	(assert (fase finalizar))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;EXPANSION DE NODO;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule acabar-busqueda
	(declare (salience 100))
	?h1 <- (fase expandir)
	?h2 <- (e $?e)
	(seleccionado ?h2)
	(test (finalp $?e))
	?h3 <- (g ?h2 ?padre ?c)
=>
	(retract ?h1)
	(assert (solucion ?padre $?e ?c)
			(fase finalizar)
	)
)

(defrule generar-hijo
	(fase expandir)
	?h1 <- (e $?e)
	(seleccionado ?h1)
	(g ?h1 ?p ?c)
	(op ?f)
	(test (aplicablep ?f  $?e))
=>
	(bind $?vals (aplicar ?f $?e))
	(bind ?coste-arco (nth (length$ $?vals) $?vals))
	(bind $?ne (subseq$ $?vals 1 (- (length $?vals) 1)))
	(bind ?hijo (assert (nuevo-e $?ne)))
	(assert (nuevo-g ?hijo ?h1 (+ ?c ?coste-arco)))
)

(defrule cerrar
	(declare (salience -100))
	?h1 <- (fase expandir)
	?h2 <- (seleccionado ?e)
	?h3 <- (frontera ?e)
=>
	(retract ?h1 ?h2 ?h3)
	(assert (explorado ?e)
			(fase extraer)
	)
)


(defrule anyadir-nuevo
	(fase expandir)
	?h1 <- (nuevo-e $?e)
	?h2 <- (nuevo-g ?h1 ?p ?c)
	(not (e $?e))
=>
	(retract ?h1 ?h2)
	(bind ?h3 (assert (e $?e)))
	(assert (frontera ?h3)
			(g ?h3 ?p ?c)
			(h ?h3 (heuristico $?e))
	)
)


(defrule eliminar-nuevo
	(fase expandir)
	?h1 <- (nuevo-e $?e)
	?h2 <-(nuevo-g ?h1 ? ?c1)
	?h3 <- (e $?e)
	(g ?h3 ? ?c2&:(<= ?c2 ?c1))
=>
	(retract ?h1 ?h2)
)

(defrule eliminar-viejo-frontera
	(fase expandir)
	?h1 <- (nuevo-e $?e)
	?h2 <- (nuevo-g ?h1 ?p1 ?c1)
	?h3 <- (e $?e)
	?h4 <- (g ?h3 ? ?c3&:(> ?c3 ?c1))
	(frontera ?h3)
=>
	(retract ?h1 ?h2 ?h4)
	(assert (g ?h3 ?p1 ?c1))
)

(defrule eliminar-viejo-explorado
	(fase expandir)
	?h1 <- (nuevo-e $?e)
	?h2 <- (nuevo-g ?h1 ?p1 ?c1)
	?h3 <- (e $?e)
	?h4 <- (g ?h3 ? ?c3&:(> ?c3 ?c1))
	?h5 <- (explorado ?h3)
 =>
	(retract ?h1 ?h2 ?h4 ?h5)
	(assert (frontera ?h3)
			(g ?h3 ?p1 ?c1)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;FASE SOLUCION;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule construir-solucion
	(fase finalizar)
	?padre <- (e $?vals)
	(g ?padre ?abuelo ?)
	?sol <- (solucion ?padre $?resto)
=>
	(retract ?sol)
	(assert (solucion ?abuelo $?vals $?resto))
)

(defrule imprimir-solucion
	(fase finalizar)
	(solucion start $?estados ?c)
=> 
	(printout t "La solucion es:" crlf)
	(printout t $?estados crlf)
	(printout t "Su coste es: " ?c crlf)
	(halt)
)

(defrule imprimir-fracaso
	(fase finalizar)
	(not (solucion $?))
=> 
	(printout t "No hay solucion" crlf)
	(halt)
)
