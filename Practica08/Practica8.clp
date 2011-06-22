;**********************;
;      IIA  10/11      ;
;      Practica 8      ;
; Miguel Gomez Gonzalo ;
;**********************;

(defmodule MAIN
	(export deftemplate ?ALL)
	(export defglobal ?ALL)
)

;	Plantillas
(deftemplate vehiculo
	(slot velocidad
		(type NUMBER)
		(default 0)
	)
	(slot aceleracion
		(type NUMBER)
		(default 0)
	)
)

(deftemplate agente
	(slot acelerador
		(type SYMBOL)
		(allowed-values off on)
	)
	(slot freno
		(type SYMBOL)
		(allowed-values off on)
	)
)

(deftemplate actualizacion
	(slot pendiente)
)

;	Variables globales
(defglobal
	?*MAX-V* = 25 
	?*MAX-T* = 500
	?*incr-ac* = 2
	?*incr-fr* = -2
)

;	Inicialización
(deffacts datos-iniciales
	(vehiculo
		(velocidad 0)
		(aceleracion 0)
	)
	(agente
		(acelerador off)
		(freno off)
	)
)

(deffacts inicio-algoritmo
	(reloj 0)
	(semilla (integer (time)))
	(actualizacion (pendiente no)))

(defrule inicio
	(semilla ?semilla)
=>
	(printout t "Hola, bienvenido al simulador" crlf)
	(printout t "Pulse retorno para avanzar en la simulacion" crlf)
	(readline)
	(seed ?semilla)
)

(defrule fin-por-velocidad
	(vehiculo
		(velocidad ?v&:(> ?v ?*MAX-V*))
		(aceleracion ?a)
	)
=>
	(printout t "VELOCIDAD MAXIMA SUPERADA." crlf)
	(halt)
)

(defrule fin-por-tiempo
	(reloj ?s&:(> ?s ?*MAX-T*))
=>
	(halt)
)

(defrule escribe-y-sigue
	(declare (salience -100))
	(actualizacion (pendiente no))
	?h-reloj <- (reloj ?s)
	(vehiculo
		(velocidad ?v)
		(aceleracion ?a)
	)
	(agente
		(acelerador ?acel)
		(freno ?fre)
	)
=>
	(format t "Tiempo: %5.1f Velocidad: %5.2f Aceleracion: %5.3f Acelerador: %3s Freno: %3s %r%n" ?s ?v ?a ?acel ?fre)
	(retract ?h-reloj)
	(assert (reloj (+ ?s 1)))
	(focus CONTROL)
)

(defrule actualizar
	(declare (salience -100))
	(actualizacion (pendiente si))
	(vehiculo
		(velocidad ?v)
		(aceleracion ?a)
	)
=>
	(focus ACTUALIZACION)
)


;	MODULO DE CONTROL
(defmodule CONTROL
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL)
)

(defrule activar-acelerador
	(vehiculo
		(velocidad ?v&:(< ?v 20))
		(aceleracion ?a)
	)
	?h-ag <- (agente)
=>
	(assert (nuevo-acelerador on))	
)

(defrule activar-freno
	(vehiculo
		(velocidad ?v&:(> ?v 20))
		(aceleracion ?a)
	)
	?h-ag <- (agente)
=>
	(assert (nuevo-freno on))	
)

(defrule desactivar-acelerador
	(vehiculo
		(velocidad ?v&:(> ?v 20))
		(aceleracion ?a)
	)
	?h-ag <- (agente)
=>
	(assert (nuevo-acelerador off))	
)

(defrule desactivar-freno
	(vehiculo
		(velocidad ?v&:(< ?v 20))
		(aceleracion ?a)
	)
	?h-ag <- (agente)
=>
	(assert (nuevo-freno off))	
)

(defrule cambio-fase
	(declare (salience -100))
	?ag <- (agente)
	?h1 <- (nuevo-acelerador ?n1)
	?h2 <- (nuevo-freno ?n2)
	?h3 <- (actualizacion (pendiente no))
=>
	(modify ?ag (acelerador ?n1)(freno ?n2))
	(modify ?h3 (pendiente si))
	(retract ?h1 ?h2)
	(return)
)

;	MODULO DE ACTUALIZACION
(defmodule ACTUALIZACION
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL)
)

(deffunction aleatorio-entre (?a ?b)
	(bind ?rango (+ 1 (- ?b ?a)))
	(+ ?a (mod (random) ?rango))
)

(defrule calcular-incr-acelerador
	(agente (acelerador on)(freno off))
=>
	(assert (incr-agente ?*incr-ac*))
)

(defrule calcular-incr-freno
	(agente (acelerador off)(freno on))
=>
	(assert (incr-agente ?*incr-fr*))
)

(defrule actualizar-vehiculo-y-cambiar-fase
	?h-vehiculo <-	(vehiculo
						(velocidad ?v)
						(aceleracion ?a)
					)
	?h-incr <- (incr-agente ?incr-agente)
	?h-act <- (actualizacion (pendiente si))
=>
	(retract ?h-incr)
	(modify ?h-vehiculo
		(velocidad (+ ?v ?incr-agente (/ (aleatorio-entre -1000 1000) 1000)))
		(aceleracion (+ ?incr-agente (/ (aleatorio-entre -1000 1000) 1000))))
	(modify ?h-act (pendiente no))
	(return)
)
