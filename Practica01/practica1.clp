;**********************;
;      IIA  10/11      ;
;      Practica 1      ;
; Miguel Gomez Gonzalo ;
;**********************;

(deftemplate luz
	(slot color (allowed-values amarillo rojo) (default ?NONE))
	(slot estado (allowed-values on off) (default off))
)

(deftemplate sensor
	(slot id (allowed-values s1 s2 s3) (default ?NONE))
	(slot estado (allowed-values izq der vert) (default ?NONE))
)

(deffacts hechos-iniciales
	(luz (color amarillo))
	(luz (color rojo))
	(sensor (id s1) (estado der))
	(sensor (id s2) (estado izq))
	(sensor (id s3) (estado der))
)

(defrule apagar-STT
	(presion alto)
	(temperatura alto)
=>
	(assert (enchufe off))
	(printout t "STT Desenchufado" crlf)
)

(defrule accionar-A1
	(presion alto)
	(temperatura normal)
=>
	(assert (a1 on))
	(printout t "Palanca A1 accionada" crlf)
)

(defrule accionar-A2-presion
	(presion un-poco-alto)
=>
	(assert (a2 on))
	(printout t "Palanca A2 accionada" crlf)
)

(defrule accionar-A2-temperatura
	(temperatura alto)
	(presion normal)
=>
	(assert (a2 on))
	(printout t "Palanca A2 accionada" crlf)
)

(defrule s1-izquierda
	(sensor (id s1) (estado izq))
=>
	(assert (presion alto))
	(printout t "Presion alta" crlf)
)

(defrule s1-vertical
	(sensor (id s1) (estado vert))
	(not (luz (color amarillo) (estado on)))
=>
	(assert (presion un-poco-alto))
	(printout t "Presion un poco alta" crlf)
)

(defrule s1-derecha
	(sensor (id s1) (estado der))
	(not (luz (color amarillo) (estado on)))
=>
	(assert (presion normal))
	(printout t "Presion normal" crlf)
)

(defrule s2-izquierda
	(sensor (id s2) (estado izq))
=>
	(assert (temperatura alto))
	(printout t "Temperatura alta" crlf)
)

(defrule s2-derecha
	(sensor (id s2) (estado der|vert))
=>
	(assert (temperatura normal))
	(printout t "Temperatura normal" crlf)
)

(defrule luz-amarilla-on
	(luz (color amarillo) (estado on))
=>
	(assert (presion alto))
	(printout t "Presion alta" crlf)
)