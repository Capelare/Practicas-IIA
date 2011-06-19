;**********************;
;      IIA  10/11      ;
;      Practica 3      ;
; Miguel Gomez Gonzalo ;
;**********************;

(deffacts hechos-iniciales
	(estado inicial)
	(acciones cafe cafe cafe leche leche azucar final)
)

(defrule servir-cafe
	?e<-(estado inicial)
	?acc<-(acciones cafe $?resto-acciones)
=>
	(assert (estado cafe))
	(retract ?e)
	(assert (acciones $?resto-acciones))
	(retract ?acc)
	(printout t "Sirviendo cafe." crlf)
)

(defrule servir-leche
	?e<-(estado inicial|cafe)
	?acc<-(acciones leche $?resto-acciones)
=>
	(assert (estado leche))
	(retract ?e)
	(assert (acciones $?resto-acciones))
	(retract ?acc)
	(printout t "Sirviendo leche." crlf)
)

(defrule servir-azucar
	?e<-(estado cafe|leche)
	?acc<-(acciones azucar $?resto-acciones)
=>
	(assert (estado azucar))
	(retract ?e)
	(assert (acciones $?resto-acciones))
	(retract ?acc)
	(printout t "Sirviendo azucar." crlf)
)

(defrule terminar
	?e<-(estado inicial|cafe|leche|azucar)
	?acc<-(acciones final)
=>
	(assert (estado final))
	(retract ?e)
	(retract ?acc)
	(printout t "Gracias por utilizar este servicio." crlf)
)

(defrule ignorar-cafe
	(estado cafe)
	?acc<-(acciones cafe $?resto-acciones)
=>
	(assert (acciones $?resto-acciones))
	(retract ?acc)
)

(defrule ignorar-leche
	(estado leche)
	?acc<-(acciones leche $?resto-acciones)
=>
	(assert (acciones $?resto-acciones))
	(retract ?acc)
)

(defrule ignorar-azucar
	(estado azucar)
	?acc<-(acciones azucar $?resto-acciones)
=>
	(assert (acciones $?resto-acciones))
	(retract ?acc)
)

(defrule error
	(declare (salience -100))
	?h-e<-(estado ?e&~error)
	?h-acc<-(acciones ?a $?resto-acciones)
=>
	(assert (estado error))
	(retract ?h-e)
	(printout t "Error en la secuencia. La accion [" ?a "] no es valida en el estado [" ?e "]" crlf)
	(retract ?h-acc)
)

(defrule error-falta-final
	(declare (salience -100))
	?h-e<-(estado ?e&~error&~final)
	?h-acc<-(acciones)
=>
	(assert (estado error))
	(retract ?h-e)
	(printout t "Error en la secuencia. Falta la accion final." crlf)
	(retract ?h-acc)
)

(defrule solicitar-secuencia
	?e<-(estado error|inicial|final)
	(not (acciones $?resto))
=>
	(retract ?e)
	(assert (estado inicial))
	(printout t "Nueva secuencia: ")
	(bind ?x (readline))
	(assert-string(format nil "(acciones %s)" ?x))
)

(defrule apagar-cafetera
	?h-e<-(estado ?e)
	?h-acc<-(acciones apagar $?resto)
=>
	(retract ?h-e)
	(assert (estado apagar))
	(retract ?h-acc)
)