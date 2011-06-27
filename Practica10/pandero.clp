
(atributo
	(nombre color) 
	(pregunta "De que color es el bicho?")
	(valores verde pardo)
)
(atributo
	(nombre tono) 
	(pregunta "Es color pardo claro o pardo oscuro?")
	(valores claro oscuro)
)
(atributo
	(nombre dedos)
	(pregunta "Tiene un numero par o impar de dedos?")
	(valores par impar)
)

(arco
	(nodo-padre t)
	(oav color pardo)
	(nodo-hijo sabandija)
)
(arco
	(nodo-padre t)
	(oav color verde)
	(nodo-hijo gambusino)
)
(arco
	(nodo-padre sabandija)
	(oav tono claro)
	(nodo-hijo aerofago)
	(solucion positivo)
)
(arco
	(nodo-padre sabandija)
	(oav tono oscuro)
	(nodo-hijo carronyero)
	(solucion negativo)
)
(arco
	(nodo-padre gambusino)
	(oav dedos impar)
	(nodo-hijo perisod)
	(solucion positivo)
)
(arco
	(nodo-padre gambusino)
	(oav dedos par)
	(nodo-hijo artiod)
	(solucion negativo)
)


(solucion
	(clase positivo)
	(mensaje "Yum! Buen provecho!")
)

(solucion
	(clase negativo)
	(mensaje "/!\\ PELIGRO, NO COMER! /!\\")
)