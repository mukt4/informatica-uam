(defun f-victor-2 (estado &optional profundidad-max f-eval)
	(let ((acciones (acciones-posibles estado)))
		(loop for accion in acciones do
			(if (dentro-del-tablero-p (estado-tablero estado) accion (- (altura-columna (estado-tablero estado) accion) 1))
				(if (not (eql (estado-turno estado) (obtener-ficha (estado-tablero estado) accion (- (altura-columna (estado-tablero estado) accion) 1))))
					(ejecutar-accion estado accion))))
		(ejecutar-accion estado (nth (random (length acciones) *my-random-state*) acciones))))

(defun f-eval-victor-2(estado)
	(random 50))

(defvar *victor-2* (make-jugador :nombre 'Victor-2
				:f-jugador #'f-victor-2
				:f-eval #'f-eval-victor-2))
