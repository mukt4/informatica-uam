; PRACTICA 1
; Autores: Tomas Higuera Viso y Manuel Chamorro Martinez De Aragon
; Version: 1.0
; Fecha: 10-2-2019

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
  ; Definimos una funcion que se encarga de realizar las operaciones recursivas
  ; Comprobamos lo primero que x no es 0 en un principio
  (if (null x)
    0
    (let ((producto-divisor (sqrt (* (producto-escalar x x) (producto-escalar y y)))))
      (if (= producto-divisor 0)
        0
        (- 1 (/ (producto-escalar x y) producto-divisor))))))

; Definimos una funcion auxiliar que realiza el producto escalar
(defun producto-escalar (x y)
  ; Caso base en el que hemos llegado al final de la lista
  (if (null x)
    0
    ; Si no hemos llegado al final de la lista realizamos las operaciones pertinentes
    (+ (* (first x) (first y)) (producto-escalar (rest x) (rest y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
  (if (null x)
    0
    (let ((producto-divisor (sqrt (* (producto-escalar-map x x) (producto-escalar-map y y)))))
      (if (= producto-divisor 0)
        0
        (- 1 (/ (producto-escalar-map x y) ))))))

; Definimos una funcion que haga el producto escalar utilizando mapcar
(defun producto-escalar-map(x y)
  (if (null x)
    0
    (reduce #'+ (mapcar #'* x y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
  ; Caso base en el que hemos terminado de evaluar la lista de vectores
  (if (null lst-of-vectors)
    nil
    ; Comprobamos si el vector a evaluar supera el nivel de confianza y debemos insertarlo
    (if (> (- 1 confidence-level) (cosine-distance-mapcar vector (first lst-of-vectors)))
      ; Si tenemos que insertarlo llamamos a una funcion que inserta el vector de manera ordenada
      (insertar-ordenadamente-coseno-distancia vector (first lst-of-vectors) (order-vectors-cosine-distance vector (rest lst-of-vectors) confidence-level))
      ; Si no tenemos que insertarlo llamamos a la funcion de manera recursiva con los vectores restantes
      (order-vectors-cosine-distance vector (rest lst-of-vectors) confidence-level))))

; Funcion que inserta de manera ordenada en funcion de la distancia del coseno
(defun insertar-ordenadamente-coseno-distancia (vector vector_insertar lista-insertar)
  ; Caso base en el que tenemos que insertar el vector al final de la lista
  (if (null (first lista-insertar))
    ; Insertamos al final de la lista
    (cons vector_insertar lista-insertar)
    ; Si no estamos en el caso base comprobamos si la distancia del coseno es mayor y tenemos que seguir llamando la funcion de manera recursiva
    (if (> (cosine-distance-mapcar vector vector_insertar) (cosine-distance-mapcar vector (first lista-insertar)))
      ; Si es menor tendremos que llamar de manera recursiva y seguir tratando de colocar el vector de manera correcta
      (cons (first lista-insertar) (insertar-ordenadamente-coseno-distancia vector vector_insertar (rest lista-insertar)))
      ; Si es mayor estamos en la posicion correcta e insertamos directamente
      (cons vector_insertar lista-insertar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
(defun get-vectors-category (categories texts distance-measure)
  ; Primero evaluamos el caso base en el cual ya se han analizado todos los textos
  (if (null texts)
    nil
    ; Si no estamos en el caso base pasamos a evaluar con que categoria se asemeja mas nuestro vector
    (mapcar #'(lambda (x) (categoria-mas-semejante categories x distance-measure)) texts)))

; Funcion que devuelve un par de id categoria y la distancia del coseno que mas se asemeja a otro vector
(defun categoria-mas-semejante (categorias texto distance-measure)
  ; Caso base en el que hemos terminado de comparar con las categorias
  (if (null categorias)
    nil
    ; Si no estamos ante el caso base tenemos que seguir comparando
    (first (sort (copy-list (mapcar #'(lambda(x) (cons (first x) (funcall distance-measure x texto))) categorias)) #'(lambda (x y) (< (rest x) (rest y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
(defun newton (f df max-iter x0 &optional (tol 0.001))
  ; Definimos el caso base en el que hemos llegado a la iteracion 0 y en el que devolvemos la semilla actual siempre
  ; y cuando sea mayor que la tolerancia de convergencia
  (if (= max-iter 0)
    nil
    ; Usamos un let para la parte recursiva de la funcion
    (let ((next (- x0 (float (/ (funcall f x0) (funcall df x0))))))
      (if (< (abs(- x0 next)) tol)
        next
        (newton f df (- max-iter 1) next tol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  ; Caso base en el que hemos acabado de analizar la lista de semillas
  (if (null semillas)
    nil
    ; Llamamos a la funcion de newton usando un let para poder guardar el valor de la misma
    ; en una variable "dinamica"
    (let ((valor (newton f df max-iter (first semillas) tol)))
      ; Si lo que nos ha devuelto la funcion de newton es null significa que no cumple con
      ; los requisitos de tolerancia
      (if (null valor)
        (one-root-newton f df max-iter (rest semillas) tol)
        valor))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  ; Caso base en el que hemos acabado de analizar la lista de semillas
  (if (null semillas)
    nil
    ; Llamamos a la funcion de newton usando un let para poder guardar el valor de la misma
    ; en una variable "dinamica"
    (let ((valor (newton f df max-iter (first semillas) tol)))
      ; Si lo que nos ha devuelto la funcion de newton es null significa que no cumple con
      ; los requisitos de tolerancia
      (if (null valor)
        ; Este cons no tiene mucho sentido, ya que la funcion puede directemete devolver la iteracion
        ; Lo unico que conseguimos aniandiendo este nil es formar una cadena de nils, esto lo hemos hecho
        ; para que la solucion sea consistente con la que nos dan en el enunciado
        (cons nil (all-roots-newton f df max-iter (rest semillas) tol))
        (cons valor (all-roots-newton f df max-iter (rest semillas) tol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas a las que les aplican un mapcan
;;; para quitar los valores nil de la lista
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla
(defun list-not-nil-roots-newton (f df max-iter semillas &optional ( tol 0.001))
  ; Usamos un let para guardar la lista en una variable "dinamica"
  (let ((lista (all-roots-newton f df max-iter semillas tol)))
    (mapcan (lambda (x) (if (not (null x)) x)) (list lista))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
  (if (null lst)
    nil
    (mapcar #'(lambda(x) (list elt x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
  (if (null lst1)
    nil
    (append (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion 
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  ; Caso base en el que nos dan una lista de listas vacia
  (if (null lstolsts)
    ; Devolvemos nil en ese caso
    nil
    ; Comrpobamos si quedan mas listas con las que combinar la primera
    (if (null (rest lstolsts))
      ; Recursion en la que combinamos la lista de listas, utilizamos auxiliarmente la funcion comnine-lst-lst
      (mapcar #'list (first lstolsts)) 
      (mapcar #'(lambda (x) (cons (first x) (first (rest x)))) (combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; defino operadores logicos
(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '!)

;; definiciones de valores de verdad, conectores y atomos
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)
  (eql x +not+))

(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))

(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

(defun bicond-connector-p (x)
  (eql x +bicond+))

(defun cond-connector-p (x)
    (eql x +cond+))

(defun and-connector-p (x)
    (eql x +and+))

(defun or-connector-p (x)
    (eql x +or+))

(defun connector-p (x)
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))

(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truth-tree
;;; Recibe una expresion y construye su arbol de verdad para
;;; determinar si es SAT o UNSAT
;;;
;;; INPUT  : fbf - Formula bien formada (FBF) a analizar.
;;; OUTPUT : T   - FBF es SAT
;;;          N   - FBF es UNSAT
;;;
(defun truth-tree (fbf)
  ; Caso base en el que la lista de fbf esta vacia
  (if (null fbf)
    ; Devuelve nil en ese caso
    nil
    ; Si el primer elemento de la base del concimiento es un literal
    (if (literal-p (first fbf))
      ; Si estamos en este caso devolvemos true
      t
      ; Si el primer elemento no es un literal procedemos a evaluar la expresion
      (truth-tree (truth-tree-aux nil fbf)))))

; PSEUDOCODIGO EJERCICIO 4
; 
; DEF F (LITERALES Y EXPRESION)
;   SI HAY CONTRADDICCIONES EN LITERALES
;      NIL
;   SI ES LITERAL EN EXPRESION
;      LITERALES + EXPRESION
;   SI LA EXPRESION ES DEL TIPO P Y Q
;      NUEVOS LITERALES = F(LITERALES Y EXPRESION(P Y Q))
;         SI NUEVOS LITERALES ES NIL
;           NIL
;         ELSE
;           RET F(NUEVOS LITERALES Y Q)
;   SI LA EXPRESION ES DEL TIPO P O Q
;       NUEVOS LITERALES = F(LITERALES Y P)
;       SI NUEVOS LITERALES ES DISTINTO DE NIL
;          RET NUEVOS LITERALES 
;       ELSE
;          RET F(LITERALES Y Q)
;   SI LA EXPRESION ES P->Q
;       EXPRESION = NOT(P O Q)
;       RET F(LITERALES Y EXPRESION)
;   SI LA EXPRESION ES P<->Q
;       EXPRESION = (P->Q) Y 

; Funcion auxilar de truth tree que evalua una expresion junto con la lista de literales de la misma
(defun truth-tree-aux (literales expresion)
  ; Si encontramos alguna contradiccion en los literales devolvemos nil
  (if (null (comprobar-literales literales))
    ; Devolvemos nil en el caso de que encontremos alguna contradiccion
    nil
    ; Si no encontramos ninguna contradiccion seguimos con la evaluacion
    ; Comprobamos si la expresion es un literal
    (if (literal-p expresion)
      ; Si el literal es un literal comprobamos si esta crea contradiccion con el resto de literales antes de devolver la
      ; nueva lista
      (if (null (comprobar-literales (append literales (list expresion))))
        ; Si hay alguna contradiccion en la lista de literales devolvemos nil
        nil
        ; Si no hay ninguna contradiccion devolvemos la lista de literales a la que le concatenamos el nuevo literal
        (append literales (list expresion)))
      ; Si el literal no es una expresion continuamos con la evaluacion
      ; Si estamos ante una puerta and
      (if (and-connector-p (first expresion))
        ; Utilizamos un let para guardar los literales obtenidos al evaluar los terminos a la derecha de la expresion
        (let ((literales-aux (truth-tree-aux literales (first (rest expresion)))))
          ; Si no obtenemos literales del primer termino devolvemos nil
          (if (null literales-aux)
            nil
            ; Si los obtenemos pasamos a comprobar el segundo termino si lo hay
            (if (= 1 (list-length (rest (rest expresion))))
              ; Si solo hay que comprobar un solo termino mas
              (truth-tree-aux literales-aux (first (rest (rest expresion))))
              ; Si hay que comprobar mas terminos
              (truth-tree-aux literales-aux (append (list (first expresion)) (rest (rest expresion)))))))
          ; Comprobamos si todos los terminos a la derecha de la expresion se cumple
        ; Si no estamos ante una puerta and comprobamos si esta es or
        (if (or-connector-p (first expresion))
          ; Realizamos la misma comprobacion que en la and pero esta vez si uno de los terminos es correct dejamos de evaluar
          (let ((literales-aux (truth-tree-aux literales (first (rest expresion)))))
            ; Si los nuevos literales devuelven null continuamos realizando comprobaciones
            (if (null literales-aux)
              (if (= 1 (list-length (rest (rest expresion))))
                (truth-tree-aux literales-aux (first (rest (rest expresion))))
                (truth-tree-aux literales-aux (append (list (first expresion)) (rest (rest expresion)))))
            ; Si los nuevos literales no son null devolvemos estos literales
            literales-aux))
          ; Si no estamos ante una puerta or seguimos comprobando
          ; Si estamos ante un condicional
          (if (cond-connector-p (first expresion))
            ; Si estamos ante un condicional realizamos la transformacion respectiva transformando en or y e primer termino negado
            (truth-tree-aux literales (list +or+ (list +not+ (first (rest expresion))) (first (rest (rest expresion)))))
            ; Si no estamos en un condicional simple comprobamos si es bicondicional
            (if (bicond-connector-p (first expresion))
              ; Si estamos ante un bicondicional realizamos la transformacion respectiva aniadiendo una and con dos condicionales
              ; simples
              (truth-tree-aux literales (list +and+ (list +cond+ (first (rest expresion)) (first (rest (rest expresion)))) (list +cond+ (first (rest (rest expresion))) (first (rest expresion)))))
              ; Si no estamos ante una bicondicional solo nos queda el caso en el que sea una puerta not
              ; Comprobamos si estamos ante una puerta not
              (if (unary-connector-p (first expresion))
                ; Comprobamos si nos encontramos ante un literal negativo despues de la puerta not
                (if (unary-connector-p (first (first (rest expresion))))
                  ; Si nos encontramos ante una puerta not delante de un literal anulamos la not
                  (truth-tree-aux literales (first (rest (first (rest expresion)))))
                  ; Si no nos encontramos ante un literal al que negamos seguimos evaluando
                  ; Comprobamos si estamos ante una puerta and
                  (if (and-connector-p (first (first (rest expresion))))
                    ; Cambiamos la and por or y negamos el resto de terminos utilizando mapcar
                    (truth-tree-aux literales (append (list +or+) (mapcar #'(lambda(x) (list +not+ x)) (rest (first (rest expresion))))))
                    ; Si no estamos ante una and comprobamos si es una or
                    (if (or-connector-p (first (first (rest expresion))))
                      ; Si estamos ante una or hacemos lo mismo que con la and pero cambiando or por and
                      (truth-tree-aux literales (append (list +and+) (mapcar #'(lambda(x) (list +not+ x)) (rest (first (rest expresion))))))
                      ; Si tampoco estamos ante or comprobamos si estamos ante una condicional
                      (if (cond-connector-p (first (first (rest expresion))))
                        ; Si estamos realizamos la transformacion necesaria de una condicional negada
                        (truth-tree-aux literales (list +not+ (list +or+ (list +not+ (first (rest (first (rest expresion))))) (first (rest (rest (first (rest expresion))))))))
                        ; Si tampoco estamos ante una condicional solo queda la opcion que estamos ante una bicondicional
                        ; realizamos las transformaciones necesarias
                        (truth-tree-aux literales (list +and+ (list +not+ (list +cond+ (first (rest (first (rest expresion)))) (first (rest (rest (first (rest expresion)))))))
                                                              (list +not+ (list +cond+ (first (rest (rest (first (rest expresion))))) (first (rest (first (rest expresion))))))))))))))))))))

; Funcion que comprueba si un conjunto de literales es correcto
; Si hay alguna contradiccion devolvemos nil
; Si no hay ninguna contradiccion devolvemos false 
(defun comprobar-literales(literales)
  ; Caso base en el que comprobamos si la lista de literales esta vacia
  (if (null literales)
    ; Si la lista de literales esta vacia devolvemos true
    t
    ; Si no lo esta comprobamos si el literal es positivo o negativo
    (if (negative-literal-p (first literales))
      ; Si el literal es negativo es necesario que comprobemos si en la lista de literales hay algun literal positivo
      ; con el mismo atom
      (if (eql (member (first(rest(first literales))) literales) nil)
        ; Si no encuentra ninguna contradiccion seguimos buscando en la lista de literales
        (comprobar-literales (rest literales))
        ; Si encontramos alguna contradiccion devolvemos nil
        nil)
      ; Si no se trata de un literal positivo procedemos a la comprobacion de si en la lista hay alggun literal negativo
      ; con el mismo  atom
      (if(eql (member (list +not+ (first literales)) literales :TEST #'EQUAL) nil)
        ; Si no encotramos nunguna contradiccion seguimos buscando en la lista de literales
        (comprobar-literales (rest literales))
        ; Si encontramos alguna contradiccion devolvemos nil
        nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Funcion que realiza busqueda en profundidad
(defun bfs (end queue net)
  ; Caso baso en el que la cola esta vacia y devolvemos una lista vacia
  (if (null queue) '() 
    ; Insercion en el path del primer elemento de la cola
    (let* ((path (first queue))
      ; Insercion en el nodo el primer elemento del path
      (node (first path)))
      ; Si el nodo actual es el nodo objetivo se devuelve la lista invertida
      (if (eql node end)
        ; Retorno de la lista invertida
        (reverse path)
      ; Si no es el nodo objetivo se continua con la recursion
      (bfs end
        ; La cola pasado por argumento contendra los caminos restantes y los caminos nuevos del nodo actual
        (append (rest queue)
        (new-paths path node net))
        net)))))

; Esta funcion devuelve todos los posibles caminos desde un nodo
(defun new-paths (path node net)
  (mapcar #'(lambda(n)
            (cons n path))
         (rest (assoc node net))))

; Esta funcion devuelve el camino mas corto
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortest-path-improved
;;; Version de busqueda en anchura que no entra en recursion
;;; infinita cuando el grafo tiene ciclos
;;; INPUT:   end: nodo final
;;;          queue: cola de nodos por explorar
;;;          net: grafo
;;; OUTPUT: camino mas corto entre dos nodos
;;;         nil si no lo encuentra

(defun bfs-improved (end queue net)
  )

(defun shortest-path-improved (end queue net)
  )