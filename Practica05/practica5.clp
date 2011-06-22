;**********************;
;      IIA  10/11      ;
;      Practica 5      ;
; Miguel Gomez Gonzalo ;
;**********************;

(deffacts inicial
	(e 0 0)
)

(deffacts operadores
	(op arr)
	(op aba)
	(op izq)
	(op der)
)

(deffunction aplicablep (?op $?coords)
	(bind ?x (nth 1 $?coords))
	(bind ?y (nth 2 $?coords))
	(switch ?op
		(case arr then TRUE)
		(case aba then (> ?y 0))
		(case izq then (> ?x 0))
		(case der then TRUE)
		(default none)
	)
)

(deffunction aplicar (?op $?coords)
	(bind ?x (nth 1 $?coords))
	(bind ?y (nth 2 $?coords))
	(switch ?op
		(case arr then (create$ ?x (+ ?y 1) 1))
		(case aba then (create$ ?x (- ?y 1) 1))
		(case izq then (create$ (- ?x 1) ?y 1))
		(case der then (create$ (+ ?x 1) ?y 1))
	)
)

(deffunction finalp ($?coords)
	(bind ?x (nth 1 $?coords))
	(bind ?y (nth 2 $?coords))
	(and (eq ?x 3) (eq ?y 3))
)

(defrule iniciar
	=>
	(assert (fase iniciar))
)

(defrule fase-iniciar-01
	?h1 <- (fase iniciar)
	?h2 <- (e $?e)
=>
	(retract ?h1)
	(assert
		(frontera ?h2)
		(g ?h2 start 0)
		(fase extraer)
	)
)

(defrule extraer-hay
	?h1 <- (fase extraer)
	?h2 <- (e $?e)
	?h3 <- (frontera ?h2)
	(g ?h2 ?p ?g)
	(not (and (frontera ?h) (g ?h ? ?g1 &:(< ?g1 ?g))))
=>
	(retract ?h1)
	(assert
		(fase expandir)
		(seleccionado ?h2)
	)
)

(defrule extraer-no-hay
	?h <- (fase extraer)
	(not (frontera ?))
=>
	(retract ?h)
	(assert (fase finalizar))
)

(defrule expandir-acabar
	(declare (salience 100))
	?h1 <- (fase expandir)
	?h2 <- (e $?e)
	?h3 <- (g ?h2 ?p ?g)
	(seleccionado ?h2)
	(test (finalp $?e))
=>
	(retract ?h1)
	(assert
		(fase finalizar)
		(solucion ?p ?e ?g)
	)
)

(defrule expandir-generar-hijo
	(fase expandir)
	?h1 <- (e $?e)
	(seleccionado ?h1)
	(g ?h1 ?p ?c)
	(op ?f)
	(test (aplicablep ?f $?e))
=>
	(bind $?hijo (aplicar ?f $?e))
	(bind ?ng (nth (length$ $?hijo) $?hijo))
	(bind $?ne (subseq$ $?hijo 1 (- (length$ $?hijo) 1)))
	(assert
		(nuevo-e $?ne)
		(nuevo-g ?h1 (+ ?c ?ng))
	)
)

(defrule anadir-nuevo-estado
	(fase expandir)
	?e <- (nuevo-e $?ne)
	(not (e $?ne))
	?g <- (nuevo-g ?p ?c)
=>
	(bind ?h3 (assert (e ?ne)))
	(assert
		(g ?h3 ?p ?c)
		(frontera ?h3)
	)
	(retract ?e ?g)
)

(defrule limpiar-nuevo-estado
	(fase expandir)
	?e <- (nuevo-e $?ne)
	(e $?ne)
	?g <- (nuevo-g ?p ?c)
=>
	(retract ?e ?g)
)

(defrule eliminar-viejo-frontera
	(fase expandir)
	?h <- (nuevo-e $?e)
	?h1 <- (nuevo-g ?p1 ?c1)
	?h2 <- (e $?e)
	(frontera ?h2)
	?h3 <- (g ?h2 ? ?c2 &:(> ?c2 ?c1))
=>
	(retract ?h ?h1 ?h3)
	(assert
		(g ?h2 ?p1 ?c1)
	)
)

(defrule cerrar-nodo
	(declare (salience -100))
	?h1 <- (fase expandir)
	?h2 <- (seleccionado ?s)
	?h3 <- (frontera ?s)
=>
	(retract ?h1 ?h2 ?h3)
	(assert
		(fase extraer)
		(explorado ?s)
	)
)

(defrule construir-camino
	(fase finalizar)
	?p1 <- (e $?p)
	(g ?p1 ?abu ?)
	?h <- (solucion ?p1 $?resto)
=>
	(retract ?h)
	(assert (solucion ?abu $?p $?resto))
)

(defrule imprimir-camino
	(fase finalizar)
	(solucion start $?estados ?c)
=>
	(printout t "Solucion: " crlf)
	(printout t $?estados crlf)
	(printout t "Coste: " crlf)
	(printout t ?c crlf)
	(halt)
)

(defrule sin-solucion
	(fase solucion)
	(not (solucion ?))
=>
	(printout t "No hay solucion" crlf)
	(halt)
)
