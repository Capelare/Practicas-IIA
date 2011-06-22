

;;;;;;;;;;;;;;;;Minimax en CLIPS;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;J.L. Pérez de la Cruz;;;;;;;;;;;;;;;
;;;;;;;Modificado por Miguel Gómez Gonzalo;;;;;;;
;;;;;;;;;;;;;;;;;;;;mayo 2011;;;;;;;;;;;;;;;;;;;;


(defrule arrancar
=>
	(focus JUEGO)
)


;	MODULO NIM

(defmodule NIM
	(export deffunction aplicar finalp f-valor)
	(export deftemplate operadores)
)

(deftemplate operadores
	(slot op1)
	(slot op2)
)

(deffacts ops-nim
	(operadores
		(op1 uno)
		(op2 dos)
	)
)

(deffunction finalp ($?t)
	(< (nth 1 $?t) 1)
)

(deffunction f-valor ($?e)
	(bind ?t (nth 1 $?e))
	(bind ?j (nth 2 $?e))
 	(if (finalp $?t) then
		(if (eq ?j max) then
			1
		else
			-1
		)
	else
		0
	)
)
	      

(deffunction aplicar ($?e)
	(bind ?op (nth 1 $?e))
	(bind ?t (nth 2 $?e))
	(bind ?j (nth 3 $?e))
	(switch ?op
		(case uno then (- ?t 1))
		(case dos then (- ?t 2))
	)
)


;	MODULO MINIMAX

(defmodule MINIMAX
	(import NIM deffunction ?ALL)
	(import NIM deftemplate operadores)
	(export deftemplate posicion recomendacion)
)

(deftemplate posicion
	(slot jugador (allowed-values max min))
	(multislot tablero)
)

(deftemplate tentativa
	(slot jugador)
	(multislot tablero)
	(slot valor)
	(slot padre)
	(slot operador-desde)
	(slot operador-hacia)
)

(deftemplate recomendacion
	(slot operador)
	(slot valor)
)


(defrule iniciar-calculo
	(posicion
		(jugador ?j)
		(tablero $?t)
	)
=>
	(assert (tentativa
				(jugador ?j)
				(tablero $?t)
				(valor nc)
				(padre raiz)
				(operador-hacia nc)
				(operador-desde raiz)
			)
	)
)

(defrule finalizar-calculo
	(tentativa
		(padre raiz)
		(valor ?v)
		(operador-hacia ?op&~nc)
	)
=>
	(assert
		(recomendacion
			(operador ?op)
			(valor ?v)
		)
	)
)


(defrule limpiar
	(declare (salience -100))
	?h <- (tentativa)
=>
	(retract ?h)
)


(defrule valor-hoja
	?h <- (tentativa
			(jugador ?j)
			(tablero $?t&:(finalp $?t))
			(valor nc)
		)
=>
	(modify ?h (valor (f-valor $?t ?j)))
)


(defrule valor-horquilla
	?h <- (tentativa
			(jugador ?j)
			(tablero $?t)
			(valor nc)
		)
	?h1 <- (tentativa
			(padre ?h)
			(operador-desde ?op1)
			(valor ?v1&~nc)
		)
	?h2 <- (tentativa
			(padre ?h)
			(operador-desde ?op2&~?op1)
			(valor ?v2&~nc)
		)
=>
	(bind ?nuevo-v
		(switch ?j
			(case max then (max ?v1 ?v2))
			(case min then (min ?v1 ?v2))
		)
	)
	(bind ?nuevo-o
		(if (eq ?nuevo-v ?v1) then
			?op1
		else
			?op2
		)
	)
	(modify ?h (valor ?nuevo-v) (operador-hacia ?nuevo-o))
)
		      

(defrule generar-tentativas
	?h <- (tentativa
			(jugador ?j)
			(valor nc)
			(tablero $?t&:(not (finalp $?t)))
		)
	(operadores
		(op1 ?op1)
		(op2 ?op2)
	)
=>
	(bind $?t1 (aplicar ?op1 $?t ?j))
	(bind $?t2 (aplicar ?op2 $?t ?j))
	(bind ?nuevo-j
		(switch ?j
			(case max then min) 
			(case min then max)
		)
	)
	(assert
		(tentativa
			(jugador ?nuevo-j)
			(padre ?h)
			(operador-desde ?op1)
			(tablero $?t1)
			(valor nc)
		)
		(tentativa
			(jugador ?nuevo-j)
			(padre ?h)
			(operador-desde ?op2)
			(tablero $?t2)
			(valor nc)
		)
	)
)

;	MODULO JUEGO
(defmodule JUEGO
	(import MINIMAX deftemplate posicion recomendacion)
	(import NIM deffunction aplicar finalp f-valor))


(deffacts inicio
	(posicion
		(jugador max)
		(tablero 5)
	)
)

(defrule seleccionar-jugada
	(posicion (jugador max))
	(not (recomendacion))
=>
	(focus MINIMAX)
)

(defrule jugada-de-max
	?p <- (posicion
			(jugador max)
			(tablero $?t)
		)
	?r <- (recomendacion (operador ?op))
=>
	(printout t "Hola, soy " max crlf)
	(printout t "La posicion es: " $?t crlf)
	(printout t "Voy a jugar: " ?op crlf)
	(bind $?nt (aplicar ?op $?t max))
	(printout t "La nueva posición es: " $?nt crlf)
	(assert
		(posicion
			(jugador min)
			(tablero $?nt)
		)
	)
    (retract ?p ?r)
)