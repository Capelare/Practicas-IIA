;**********************;
;      IIA  10/11      ;
;     Practica  11     ;
; Miguel Gomez Gonzalo ;
;**********************;

(defmodule MAIN
	(export ?ALL)
)

(deffunction preguntar (?mensaje $?opciones)
	"muestra ?mensaje y lee la respuesta hasta que este en ?opciones"
	(printout t crlf ?mensaje " " $?opciones  "? ")
	(bind ?result (read))
	(while (not (member$ ?result ?opciones)) do
		(printout t crlf ?mensaje " " $?opciones  "? ")
		(bind ?result (read))
	)
	?result
)

(deftemplate atributo
	(slot nombre (type SYMBOL) (default ?NONE))
	(slot pregunta (type STRING) (default ""))
	(multislot valores)
)

(deftemplate arco
	(slot nodo-padre (type SYMBOL))
	(multislot oav (cardinality 2 2))
	(slot nodo-hijo (type SYMBOL))
	(slot solucion (default FALSE))
)

(deftemplate solucion
	(slot clase) 
	(slot mensaje)
)

(deftemplate solucion-hallada
	(slot nodo)
)

(defrule inicio
=>
	(printout t "Fichero de arbol: ")
	(bind ?f (readline))
	(load-facts ?f)
	(focus CONSULTA)
)

(defrule comprobar-solucion
	(solucion-hallada)
=>
	(bind ?r (preguntar "Es correcta la solucion" si no))
	(if (eq ?r no) 
		then (focus REMENDAR)
	)
)
  
(defrule sin-solucion
	(declare (salience -100))
	(not (solucion-hallada))
=>
	(printout t "Lo siento, no ha sido posible hallar ninguna solucion" crlf)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule CONSULTA
	(import MAIN ?ALL)
)

(defrule inicio
=>
	(assert (deducido t))
)


(defrule preguntar 
	(deducido ?c)
	(arco (nodo-padre ?c) (oav ?a ?))
	(atributo (nombre ?a) (pregunta ?p) (valores $?vs))
	(not (preguntadop ?a))
=>
	(bind ?r (preguntar ?p ?vs))
	(assert (preguntadop ?a))
	(assert (oav ?a ?r))
)

(defrule refinar
	(deducido ?c1)
	(oav ?a ?v)
	(arco
		(nodo-padre ?c1)
		(oav ?a ?v)
		(nodo-hijo ?c2)
		(solucion FALSE)
	)
=> 
	(assert (deducido ?c2))
)

(defrule dar-solucion
	(deducido ?c1)
	(oav ?a ?v)
	(arco
		(nodo-padre ?c1)
		(oav ?a ?v)
		(nodo-hijo ?c2)
		(solucion ?s&~FALSE)
	)
	(solucion (clase ?s) (mensaje ?msj))
=> 
	(assert (solucion-hallada (nodo ?c2)))
	(printout t "Deducida la solucion: " ?msj crlf)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule REMENDAR
	(import MAIN ?ALL)
)

(defrule remendar1
	(solucion-hallada (nodo ?n))
	?h<-(arco (nodo-hijo ?n) (solucion ?s))
   	(solucion (clase ?s))
	(solucion (clase ?otra-s&~?s))
=>
	(printout t "Que nuevo atributo permite decir que la solucion dada es erronea?" crlf)
	(bind ?a (read))
	(printout t "Que valor del atributo " ?a " corresponde al caso?" crlf)
	(bind ?v1 (read))
	(printout t "Que otro valor puede tomar el atributo " ?a "?" crlf)
	(bind ?v2 (read))
	(printout t "Que pregunta hay que hacer para el atributo " ?a "?" crlf)
	(bind ?cad (readline))
	(assert (atributo
		(nombre ?a)
		(valores ?v1 ?v2)
		(pregunta ?cad))
	)
	(modify ?h (solucion FALSE))
	(assert
		(arco
			(nodo-padre ?n)
			(nodo-hijo (gensym))
			(oav ?a ?v1)
			(solucion ?otra-s)
		)
		(arco
			(nodo-padre ?n)
			(nodo-hijo (gensym))
			(oav ?a ?v2)
			(solucion ?s)
		)
	)
	(printout t "Nuevo fichero de arbol: ")
	(bind ?f (readline))
	(save-facts ?f visible atributo arco solucion)
)