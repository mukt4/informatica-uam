(defpackage :conecta4
  (:use :common-lisp)
  (:export :copy-array
	   :tablero
	   :tablero-ancho
	   :tablero-alto
	   :estado
	   :estado-tablero
	   :estado-turno
	   :jugador
	   :make-jugador
	   :jugador-nombre
	   :copiar-tablero 
	   :muestra-tablero 
	   :columnas-jugables 
	   :poner-ficha 
	   :obtener-ficha 
	   :altura-columna 
	   :ganador-tablero 
	   :dentro-del-tablero-p
	   :contar-abajo 
	   :contar-arriba 
	   :contar-derecha
	   :contar-izquierda 
	   :contar-abajo-derecha 
	   :contar-abajo-izquierda 
	   :contar-arriba-derecha 
	   :contar-arriba-izquierda 
	   :copiar-estado 
	   :acciones-posibles 
	   :siguiente-jugador 
	   :ejecutar-accion 
	   :generar-sucesores
	   :juego-terminado-p
	   :tablas-p 
	   :ganador 
	   :elegir-accion
	   :*verbose*
	   :partida
	   :f-jugador-aleatorio
	   :f-jugador-negamax
	   :f-jugador-humano
	   :f-no-eval
	   :f-eval-aleatoria
	   :+val-min+
	   :+val-max+
	   :*my-random-state*
           :negamax
	   ))

(in-package conecta4)

;; SBL
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning)))
;(defmacro my-with-timeout ((seconds &body timeout-body) &body body)
;  `(handler-case
;      (sb-ext:with-timeout ,seconds ,@body)
;      (sb-ext:timeout (e) ,@timeout-body))))

;; Allegro 6
;;(defmacro my-with-timeout  ((seconds &body timeout-body) &body body)
;;   `(mp:with-timeout (,seconds ,@timeout-body) ,@body))

;; Allegro 10
;(defmacro my-with-timeout  ((seconds &body timeout-body) &body body)
;  `(sys:with-timeout (,seconds ,@timeout-body) ,@body))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defvar *verbose* t)
(defvar *my-random-state* (make-random-state t))

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

(defstruct (tablero
 (:constructor make-tablero (&key (alto 6)
				  (ancho 7)
				  (casillas (make-array (list alto ancho) :initial-element nil))))
 ;(:copier copiar-tablero)
  )
  alto
  ancho
  casillas)

(defun copiar-tablero (tablero)
  (make-tablero :alto (tablero-alto tablero)
		:ancho (tablero-ancho tablero)
		:casillas (copy-array (tablero-casillas tablero))))

(defun muestra-tablero (tablero)
  (format t "~%~%")
  (loop for j from 0 below (tablero-ancho tablero) do
	(format t " [~S]" j))
  (format t "~%")
  (loop for i downfrom (1- (tablero-alto tablero)) to 0 do
	(loop for j from 0 below (tablero-ancho tablero) do
	      (format t "· · "))
	(format t "·~%")
	(loop for j from 0 below (tablero-ancho tablero) do
	      (let ((ficha (aref (tablero-casillas tablero) i j)))
		(if (not ficha)
		    (format t "·   ")
		  (format t "· ~S " ficha))))
	(format t "·~%"))
  (loop for j from 0 below (tablero-ancho tablero) do
	(format t "· · "))
  (format t "·~%")
  (loop for j from 0 below (tablero-ancho tablero) do
	(format t " [~S]" j))
  (format t "~%"))

(defun columnas-jugables (tablero)
  (let (columnas-libres)
    (loop for i from (1- (tablero-ancho tablero)) downto 0 do
	  (if (null (aref (tablero-casillas tablero) (1- (tablero-alto tablero)) i))
	      (setf columnas-libres (cons i columnas-libres))))
    columnas-libres))

(defun poner-ficha (tablero columna ficha)
  (let ((altura (altura-columna tablero columna)))
    (setf (aref (tablero-casillas tablero) altura columna) ficha)))

(defun obtener-ficha (tablero columna fila)
  (aref (tablero-casillas tablero) fila columna))

(defun altura-columna (tablero columna)
  (loop for altura from 0 below (tablero-alto tablero) do
	(if (null (aref (tablero-casillas tablero) altura columna))
	    (return-from altura-columna altura)))
  (tablero-alto tablero))

(defun ganador-tablero (tablero)
  (loop for columna from 0 below (tablero-ancho tablero) do
	(let* ((altura (altura-columna tablero columna)))
	  (if (> altura 0)
	      (let* ((fila (1- altura))
		     (ficha (obtener-ficha tablero columna fila)))
		(cond ((>= (contar-horizontal tablero ficha columna fila) 4)
		       (return-from ganador-tablero ficha))
		      ((>= (contar-vertical tablero ficha columna fila) 4)
		       (return-from ganador-tablero ficha))
		      ((>= (contar-diagonal-ascendente tablero ficha columna fila) 4)
		       (return-from ganador-tablero ficha))
		      ((>= (contar-diagonal-descendente tablero ficha columna fila) 4)
		       (return-from ganador-tablero ficha))))))))

(defun contar-horizontal (tablero ficha columna fila)
  (+ (contar-derecha tablero ficha columna fila)
     (contar-izquierda tablero ficha (1- columna) fila)))

(defun contar-vertical (tablero ficha columna fila)
  (+ (contar-abajo tablero ficha columna fila)
     (contar-arriba tablero ficha columna (1+ fila))))
  
(defun contar-diagonal-ascendente (tablero ficha columna fila)
  (+ (contar-abajo-izquierda tablero ficha columna fila)
     (contar-arriba-derecha tablero ficha (1+ columna) (1+ fila))))
  
(defun contar-diagonal-descendente (tablero ficha columna fila)
  (+ (contar-abajo-derecha tablero ficha columna fila)
     (contar-arriba-izquierda tablero ficha (1- columna) (1+ fila))))

;; ----------------------------------------------------------------------------------------

(defun dentro-del-tablero-p (tablero columna fila)
  (and (>= columna 0)
       (>= fila 0)
       (< columna (tablero-ancho tablero))
       (< fila (tablero-alto tablero))))

;(trace dentro-del-tablero-p)

(defun contar-abajo (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-abajo tablero ficha columna (1- fila)))))

(defun contar-arriba (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-arriba tablero ficha columna (1+ fila)))))

(defun contar-derecha (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-derecha tablero ficha (1+ columna) fila))))

(defun contar-izquierda (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-izquierda tablero ficha (1- columna) fila))))

(defun contar-abajo-derecha (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-abajo-derecha tablero ficha (1+ columna) (1- fila)))))

(defun contar-abajo-izquierda (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-abajo-izquierda tablero ficha (1- columna) (1- fila)))))

(defun contar-arriba-derecha (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-arriba-derecha tablero ficha (1+ columna) (1+ fila)))))

(defun contar-arriba-izquierda (tablero ficha columna fila)
  (if (or (not (dentro-del-tablero-p tablero columna fila))
	  (not (eql (obtener-ficha tablero columna fila) ficha)))
      0
    (1+ (contar-arriba-izquierda tablero ficha (1- columna) (1+ fila)))))

;; ----------------------------------------------------------------------------------------

(defstruct estado
  (turno 0) 
  (tablero (make-tablero)))

(defun copiar-estado (estado)
  (let ((tablero (estado-tablero estado)))
    (make-estado :turno (estado-turno estado)
		 :tablero (make-tablero :alto (tablero-alto tablero)
					:ancho (tablero-ancho tablero)
					:casillas (copy-array (tablero-casillas tablero))))))

(defun acciones-posibles (estado)
  (columnas-jugables (estado-tablero estado)))

(defun siguiente-jugador (jugador)
  (if (eql jugador 0) 1 0))

(defun ejecutar-accion (estado accion)
  (let ((jugador (estado-turno estado))
	(nuevo-estado (copiar-estado estado)))
    (poner-ficha (estado-tablero nuevo-estado) accion jugador)
    (setf (estado-turno nuevo-estado) (siguiente-jugador jugador))
    nuevo-estado))

(defun generar-sucesores (estado)
  (mapcar #'(lambda (accion) (ejecutar-accion estado accion))
	  (acciones-posibles estado)))

(defun juego-terminado-p (estado)
  (or (ganador estado) (tablas-p estado)))

(defun tablas-p (estado)
  (and (not (ganador estado)) (null (acciones-posibles estado))))

(defun ganador (estado)
  (ganador-tablero (estado-tablero estado)))

;; ------------------------------------------------------

(defstruct jugador
  nombre
  f-jugador
  f-eval)

;; ------------------------------------------------------

(defun f-jugador-aleatorio (estado &optional profundidad-max f-eval)
  (let ((acciones (acciones-posibles estado)))
    (ejecutar-accion estado (nth (random (length acciones) *my-random-state*) acciones))))

(defun f-jugador-erroneo (estado &optional profundidad-max f-eval)
  (/ 1 0))

(defun f-jugador-negamax (estado &optional profundidad-max f-eval)
  (when *verbose*
    (muestra-tablero (estado-tablero estado)))
  (negamax estado t profundidad-max f-eval))

(defun f-jugador-humano (estado &optional profundidad-max f-eval)
  (let ((acciones (acciones-posibles estado)))
    (muestra-tablero (estado-tablero estado))
    (format t "~%Jugador ~S> " (estado-turno estado))
    (ejecutar-accion estado (elegir-accion acciones))))

;; Abandond move not implemented
(defun elegir-accion (acciones)
  (format t "Elije columna ~A: " acciones)
  (force-output)
  (let ((accion (read)))
    (if (member accion acciones)
	accion
      (elegir-accion acciones))))

;; -----------------------------------------------------------------------

(defun partida (jugador0 jugador1 &optional (profundidad-max 4))
  (let ((estado-inicial (make-estado)))
    (jugar estado-inicial jugador0 jugador1 profundidad-max)))

(defun jugar (estado jugador0 jugador1 profundidad-max)
  (loop
   (if *verbose*
       (muestra-tablero (estado-tablero estado)))
   (cond ((juego-terminado-p estado)
	  (return-from jugar (ganador estado)))
	 ((eql (estado-turno estado) 0)
	  (setf estado (funcall (jugador-f-jugador jugador0)
				estado
				profundidad-max
				(jugador-f-eval jugador0))))
	 (t
	  (setf estado (funcall (jugador-f-jugador jugador1)
				estado
				profundidad-max
				(jugador-f-eval jugador1)))))))

;; ------------------------------------------------------------------------

(defparameter +val-min+ -10000000)
(defparameter +val-max+ (- +val-min+))

(defun negamax (estado devolver-estado profundidad-max f-eval)
  (negamax-1 estado devolver-estado 0 profundidad-max f-eval))

(defun negamax-1 (estado devolver-estado profundidad profundidad-max f-eval)
  (if (>= profundidad profundidad-max)
      (unless devolver-estado (funcall f-eval estado))
    (let ((estados-sucesores (generar-sucesores estado)))
      (if (null estados-sucesores)
	  (unless devolver-estado (funcall f-eval estado))
	(let ((mejor-valor +val-min+)
	      (mejor-sucesor))
	  (loop for estado-sucesor in estados-sucesores do
		(let ((valor (- (negamax-1 estado-sucesor nil (1+ profundidad) profundidad-max f-eval))))
		  (when (>= valor mejor-valor)
		    (setf mejor-valor valor)
		    (setf mejor-sucesor estado-sucesor))))
	  (if devolver-estado mejor-sucesor mejor-valor))))))

;; ------------------------------------------------------------------------

;; Funciones de evaluación de un estado

(defun f-eval-aleatoria (estado)
  (random 100))

(defun f-eval-erronea (estado)
  (/ 1 0))

(defun f-no-eval (estado)
  0)

;;
