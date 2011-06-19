;**********************;
;      IIA  10/11      ;
;      Practica 4      ;
; Miguel Gomez Gonzalo ;
;**********************;

(defglobal
	?*max-g* = 4
	?*max-c* = 3
	?*final-g* = 2
)

(deftemplate nodo
	(slot j-g (type INTEGER) (default 0))
	(slot j-c (type INTEGER) (default 0))
	(slot padre (type FACT-ADDRESS SYMBOL) (allowed-symbols start))
	(slot regla (type SYMBOL) (allowed-values start llenar-c llenar-g vaciar-c-en-g vaciar-g-en-c vaciar-g vaciar-c))
)

(deffacts raiz
	(nodo
		(padre start)
		(regla start)
	)
)

(deffunction finalp (?g ?c)
	(eq ?g ?*final-g*)
)

(defrule borrar-duplicados
	(declare (salience 100))
	?n1<-(nodo
		(j-g ?g)
		(j-c ?c)
	)
	?n2<-(nodo
		(j-g ?g)
		(j-c ?c)
	)
	(test (> (fact-index ?n2) (fact-index ?n1)))
=>
	(retract ?n2)
)

(defrule llenar-g
	?n<-(nodo
		(j-g ?g&:(< ?g ?*max-g*))
		(j-c ?c)
	)
=>
	(assert (nodo
		(j-g ?*max-g*)
		(j-c ?c)
		(padre ?n)
		(regla llenar-g)
	))
)

(defrule llenar-c
	?n<-(nodo
		(j-g ?g)
		(j-c ?c&:(< ?c ?*max-c*))
	)
=>
	(assert (nodo
		(j-g ?g)
		(j-c ?*max-c*)
		(padre ?n)
		(regla llenar-c)
	))
)

(defrule vaciar-g
	?n<-(nodo
		(j-g ?g&:(> ?g 0))
		(j-c ?c)
	)
=>
	(assert (nodo
		(j-g 0)
		(j-c ?c)
		(padre ?n)
		(regla vaciar-g)
	))
)

(defrule vaciar-c
	?n<-(nodo
		(j-g ?g)
		(j-c ?c&:(> ?c 0))
	)
=>
	(assert (nodo
		(j-g ?g)
		(j-c 0)
		(padre ?n)
		(regla vaciar-c)
	))
)

(defrule vaciar-g-en-c
	?n<-(nodo
		(j-g ?g&:(> ?g 0))
		(j-c ?c&:(< ?c ?*max-c*))
	)
=>
	(assert (nodo
		(j-g (max 0 (- ?g (- ?*max-c* ?c))))
		(j-c (min ?*max-c* (+ ?c ?g)))
		(padre ?n)
		(regla vaciar-g-en-c)))
)

(defrule vaciar-c-en-g
	?n<-(nodo
		(j-g ?g&:(< ?g ?*max-g*))
		(j-c ?c&:(> ?c 0))
	)
=>
	(assert (nodo
		(j-c (max 0 (- ?c (- ?*max-g* ?g))))
		(j-g (min ?*max-g* (+ ?c ?g)))
		(padre ?n)
		(regla vaciar-c-en-g)))
)

(defrule acabar
	(declare (salience 100))
	(nodo
		(j-g ?g)
		(j-c ?c)
		(padre ?p)
		(regla ?r)
	)
	(test (finalp ?g ?c))
=>
	(assert (solucion ?p ?r fin))
)

(defrule construir-solucion
	(declare (salience 100))
	?n<-(nodo
		(regla ?r)
		(padre ?p)
	)
	?s<-(solucion ?n $?rules)
=>
	(retract ?s)
	(assert (solucion ?p ?r $?rules))
)

(defrule acabar-solucion
	(declare (salience 100))
	?s<-(solucion start $?rules)
=>
	(printout t $?rules)
	(halt)
)
