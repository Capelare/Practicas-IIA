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
	(slot humo (type NUMBER) (default 0))
	(slot ventana (type SYMBOL) (allowed-values abierta cerrada) (default cerrada))
	(slot puerta (type SYMBOL) (allowed-values abierta cerrada) (default cerrada))
)

(deftemplate actualizacion
	(slot pendiente)
)

;;;Variables globales
(defglobal ?*MAX-T* = 30)

;;;Inicialización
(deffacts datos-iniciales
	(habitacion
		(temperatura 21)
		(humo 0)
		(ventana cerrada)
		(puerta cerrada)
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
	(habitacion (temperatura ?t)(humo ?h)(ventana ?v)(puerta ?p))
=>
	(format t "Tiempo: %5.1f Temperatura: %3.2f Humo: %3.2f Ventana: %7s Puerta: %7s %r%n" ?s ?t ?h ?v ?p)
	(retract ?h-reloj)
	(assert (reloj (+ ?s 1)))
	(focus CONTROL)
)

(defrule actualizar
	(declare (salience -100))
	(actualizacion (pendiente si))
	(habitacion (temperatura ?t)(humo ?h)(ventana ?v)(puerta ?p))
=>
	(focus ACTUALIZACION)
)

(defmodule CONTROL
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL)
)

(defrule abrir-ventana
	(habitacion (temperatura ?t&:(> ?t 22)))
=>
	(assert (vent abierta))	
)

(defrule abrir-puerta-vent-cerrada
	(vent cerrada)
=>
	(assert (puer abierta))
)

(defrule cerrar-ventana
	(habitacion (temperatura ?t&:(< ?t 19)))
=>
	(assert (vent cerrada))	
)

(defrule cerrar-puerta-vent-abierta
	(vent abierta)
=>
	(assert (puer cerrada))
)

(defrule no-hacer-nada
	(habitacion
		(temperatura ?t&:(<= ?t 22)&:(>= ?t 19))
		(ventana ?v)
	)
=>
	(assert (vent ?v))
)

(defrule cambio-fase
	(declare (salience -100))
	?hab <- (habitacion)
	?h1 <- (vent ?v)
	?h2 <- (puer ?p)
	?h3 <- (actualizacion (pendiente no))
=>
	(modify ?hab (ventana ?v)(puerta ?p))
	(modify ?h3 (pendiente si))
	(retract ?h1 ?h2)
	(return)
)

(defmodule ACTUALIZACION
	(import MAIN deftemplate ?ALL)
	(import MAIN defglobal ?ALL)
)

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

(defrule calcular-decr-humo-puerta
	(habitacion (ventana cerrada)(puerta abierta)(humo ?h))
=>
	(assert (cambio-humo (- (/ (aleatorio-entre 1000 2000) 100) (* ?h 0.1))))
)

(defrule calcular-decr-humo-ventana
	(habitacion (ventana abierta)(puerta cerrada)(humo ?h))
=>
	(assert (cambio-humo (- (/ (aleatorio-entre 1000 2000) 100) (* ?h 0.25))))
)

(defrule calcular-incr-humo
	(habitacion (ventana cerrada)(puerta cerrada) (humo ?h))
=>
	(assert (cambio-humo (/ (aleatorio-entre 1000 2000) 100)))
)

(defrule actualizar-habitacion-y-cambiar-fase
	?h-habitacion <- (habitacion(temperatura ?t)(humo ?h))
	?h-cambio-t <- (cambio-temp ?cambio-temp)
	?h-cambio-h <- (cambio-humo ?cambio-humo)
	?h-act <- (actualizacion (pendiente si))
=>
	(retract ?h-cambio-t ?h-cambio-h)
	(modify ?h-habitacion (temperatura (+ ?t ?cambio-temp)) (humo (+ ?h ?cambio-humo)))
	(modify ?h-act (pendiente no))
	(return)
)

