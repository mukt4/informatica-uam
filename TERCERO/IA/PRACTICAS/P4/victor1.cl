(defpackage :2363_P08_3ca0f ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :f-eval-victor-1 :*alias*)) ; exporta la función de evaluación y un alias
4
(in-package 2363_P08_3ca0f)
(defvar *alias* '|Victor 1|) ; alias que aparece en el ranking
(defun heuristica (estado) ; función de evaluación heurística
	(let ((acciones (acciones-posibles estado)))
		(loop for accion in acciones do
			(if (dentro-del-tablero-p (estado-tablero estado) accion (- (altura-columna (estado-tablero estado) accion) 1))
				(if (eql (estado-turno estado) (obtener-ficha (estado-tablero estado) accion (- (altura-columna (estado-tablero estado) accion) 1)))
					(ejecutar-accion estado accion))))
		(ejecutar-accion estado (nth (random (length acciones) *my-random-state*) acciones))))