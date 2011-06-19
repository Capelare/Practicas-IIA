;**********************;
;      IIA  10/11      ;
;      Practica 2      ;
; Miguel Gomez Gonzalo ;
;**********************;

(deftemplate conjunto
	(slot nombre)
	(multislot elems)
)

(deffacts hechos-iniciales
	(conjunto
		(nombre C1)
		(elems a b c d e)
	)
	(conjunto
		(nombre C2)
		(elems g f c b a)
	)
	(operacion C3 es C1 interseccion C2)
)

(defrule calcular-union
	(conjunto
		(nombre ?N1)
		(elems $?elems1)
	)
	(conjunto
		(nombre ?N2)
		(elems $?elems2)
	)
	?o<-(operacion ?N3 es ?N1 union ?N2)
=>
	(assert
		(conjunto
			(nombre ?N3)
			(elems $?elems1 $?elems2)
		)
	)
	(assert (operacion simplificar-union ?N3))
	(retract ?o)
)

(defrule calcular-interseccion
	(conjunto
		(nombre ?N1)
		(elems $?elems1)
	)
	(conjunto
		(nombre ?N2)
		(elems $?elems2)
	)
	?o<-(operacion ?N3 es ?N1 interseccion ?N2)
=>
	(assert
		(conjunto
			(nombre ?N3)
			(elems $?elems1 $?elems2)
		)
	)
	(assert (operacion simplificar-interseccion ?N3))
	(retract ?o)
)

(defrule simplificar-interseccion
	?c1<-(conjunto
			(nombre ?N1)
			(elems $?es1 ?x $?es2)
		 )
	(operacion simplificar-interseccion ?N1)
	(test (not (or (member$ ?x ?es1) (member$ ?x ?es2))))
=>
	(modify ?c1 (elems $?es1 $?es2))
)

(defrule finalizar-simplificar-interseccion
	(declare (salience -100))
	?a<-(operacion simplificar-interseccion ?N1)
=>
	(assert (operacion simplificar-union ?N1))
	(retract ?a)
)

(defrule simplificar-union
	?c1<-(conjunto
			(nombre ?N1)
			(elems $?es1 ?x $?es2)
		 )
	(operacion simplificar-union ?N1)
	(test (member$ ?x ?es2))
=>
	(modify ?c1 (elems $?es1 $?es2))
)

(defrule finalizar-simplificar-union
	(declare (salience -100))
	?a<-(operacion simplificar-union ?N1)
=>
	(retract ?a)
)