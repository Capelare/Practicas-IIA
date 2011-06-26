;**********************;
;      IIA  10/11      ;
;     Practica  11     ;
; Miguel Gomez Gonzalo ;
;**********************;

(defmodule MAIN
	(export deftemplate ?ALL)
	(export defglobal ?ALL))

;;;Plantillas
(deftemplate habitacion
	(slot temperatura (type NUMBER) (default 0))
	(slot ventana (type SYMBOL) (allowed-values abierta cerrada) (default cerrada))
)

(deftemplate actualizacion
	(slot pendiente))

;;;Variables globales
(defglobal ?*MAX-T* = 30)

;;;Inicialización
(deffacts datos-iniciales
	(habitacion
		(temperatura 21)
		(ventana cerrada)
	)
)

(deffacts inicio-algoritmo
	(reloj 0)
	(semilla (integer (time)))
	(actualizacion (pendiente no))
)

(defrule inicio
	(semilla ?semilla)
=>
	(printout t "Hola, bienvenido al simulador" crlf)
	(printout t "Pulse retorno para avanzar en la simulacion" crlf)
	(readline)
	(seed ?semilla)
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
	(habitacion (temperatura ?t)(ventana ?v))
=>
	(format t "Tiempo: %5.1f Temperatura: %5.2f Ventana: %7s %r%n" ?s ?t ?v)
	(retract ?h-reloj)
	(assert (reloj (+ ?s 1)))
	(focus CONTROL)
)

(defrule actualizar
	(declare (salience -100))
	(actualizacion (pendiente si))
	(habitacion (temperatura ?t)(ventana ?v))
=>
	(focus ACTUALIZACION)
)

(defmodule CONTROL
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL))

(defrule abrir-ventana
	(habitacion (temperatura ?t&:(> ?t 22)))
=>
	(assert (vent abierta))	
)

(defrule cerrar-ventana
	(habitacion (temperatura ?t&:(< ?t 19)))
=>
	(assert (vent cerrada))	
)

(defrule no-hacer-nada
	(habitacion (temperatura ?t&:(<= ?t 22)&:(>= ?t 19))(ventana ?v))
=>
	(assert (vent ?v))
)

(defrule cambio-fase
	(declare (salience -100))
	?hab <- (habitacion)
	?h1 <- (vent ?n1)
	?h2 <- (actualizacion (pendiente no))
=>
	(modify ?hab (ventana ?n1))
	(modify ?h2 (pendiente si))
	(retract ?h1)
	(return)
)

(defmodule ACTUALIZACION
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL))

(deffunction aleatorio-entre (?a ?b)
	(bind ?rango (+ 1 (- ?b ?a)))
	(+ ?a (mod (random) ?rango))
)

(defrule calcular-decr-temp
	(habitacion (ventana abierta) (temperatura ?t))
=>
	(assert (cambio-temp (- 0 (/ (- ?t (/ (aleatorio-entre 2000 8000) 1000)) 5))))
)

(defrule calcular-incr-temp
	(habitacion (ventana cerrada) (temperatura ?t))
=>
	(assert (cambio-temp (/ (- 36 ?t) 10)))
)

(defrule actualizar-habitacion-y-cambiar-fase
	?h-habitacion <- (habitacion(temperatura ?t)(ventana ?v))
	?h-cambio <- (cambio-temp ?cambio-temp)
	?h-act <- (actualizacion (pendiente si))
=>
	(retract ?h-cambio)
	(modify ?h-habitacion (temperatura (+ ?t ?cambio-temp)))
	(modify ?h-act (pendiente no))
	(return)
)

