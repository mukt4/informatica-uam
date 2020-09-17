(defun f-fabrik (estado &optional profundidad-max f-eval)
	(let ((acciones (acciones-posibles estado)))
		(loop for accion in acciones do
			(if (not (dentro-del-tablero-p (estado-tablero estado) accion (- (altura-columna (estado-tablero estado) accion) 1)))
					(ejecutar-accion estado accion)))
		(ejecutar-accion estado (nth (random (length acciones) *my-random-state*) acciones))))

(defun f-eval-fabrik(estado)
	(random 50))

(defvar *fabrik* (make-jugador :nombre 'Fabrik
				:f-jugador #'f-fabrik
				:f-eval #'f-eval-fabrik))