;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;
;;    Created:  Tomas Higuera Viso y Manuel Chamorro Martinez de Aragon
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cities* '(Calais Reims Paris Nancy Orleans 
                                St-Malo Brest Nevers Limoges 
                                Roenne Lyon Toulouse Avignon Marseille))

(defparameter *trains*
  '((Paris Calais (34.0 60.0))      (Calais Paris (34.0 60.0))
    (Reims Calais (35.0 70.0))      (Calais Reims (35.0 70.0))
    (Nancy Reims (35.0 55.0))       (Reims Nancy (35.0 55.0))
    (Paris Nancy (40.0 67.0))       (Nancy Paris (40.0 67.0))
    (Paris Nevers (48.0 75.0))      (Nevers Paris (48.0 75.0))
    (Paris Orleans (23.0 38.0))     (Orleans Paris (23.0 38.0))
    (Paris St-Malo (40.0 70.0))     (St-Malo Paris (40.0 70.0))
    (St-Malo Nantes (20.0 28.0))    (Nantes St-Malo (20.0 28.0))
    (St-Malo Brest (30.0 40.0))     (Brest St-Malo (30.0 40.0))
    (Nantes Brest (35.0 50.0))      (Brest Nantes (35.0 50.0))
    (Nantes Orleans (37.0 55.0))    (Orleans Nantes (37.0 55.0))
    (Nantes Toulouse (80.0 130.0))  (Toulouse Nantes (80.0 130.0))
    (Orleans Limoges (55.0 85.0))   (Limoges Orleans (55.0 85.0))
    (Limoges Nevers (42.0 60.0))    (Nevers Limoges (42.0 60.0))
    (Limoges Toulouse (25.0 35.0))  (Toulouse Limoges (25.0 35.0))
    (Toulouse Lyon (60.0 95.0))     (Lyon Toulouse (60.0 95.0))
    (Lyon Roenne (18.0 25.0))       (Roenne Lyon  (18.0 25.0))
    (Lyon Avignon (30.0 40.0))      (Avignon Lyon (30.0 40.0))
    (Avignon Marseille (16.0 25.0)) (Marseille Avignon (16.0 25.0))
    (Marseille Toulouse (65.0 120.0)) (Toulouse Marseille (65.0 120.0)))) 
    

(defparameter *canals*
  '((Reims Calais (75.0 15.0)) (Paris Reims (90.0 10.0))
    (Paris Nancy (80.0 10.0)) (Nancy reims (70.0 20.0))
    (Lyon Nancy (150.0 20.0)) (Nevers Paris (90.0 10.0))
    (Roenne Nevers (40.0 5.0)) (Lyon Roenne (40.0 5.0))
    (Lyon Avignon (50.0 20.0)) (Avignon Marseille (35.0 10.0))
    (Nantes St-Malo (40.0 15.0)) (St-Malo Brest (65.0 15.0))
    (Nantes Brest (75.0 15.0))))



(defparameter *estimate* 
  '((Calais (0.0 0.0)) (Reims (25.0 0.0)) (Paris (30.0 0.0)) 
    (Nancy (50.0 0.0)) (Orleans (55.0 0.0)) (St-Malo (65.0 0.0))
    (Nantes (75.0 0.0)) (Brest (90.0 0.0)) (Nevers (70.0 0.0)) 
    (Limoges (100.0 0.0)) (Roenne (85.0 0.0)) (Lyon (105.0 0.0))
    (Toulouse (130.0 0.0)) (Avignon (135.0 0.0)) (Marseille (145.0 0.0))))

(defparameter *origin* 'Marseille)

(defparameter *destination* '(Calais))

(defparameter *forbidden*  '(Avignon))

(defparameter *mandatory* '(Paris))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristics
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state (time-est cost-est) )
;;             where the first element is the name of a state and the second
;;             a number estimating the costs to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
;;  It is necessary to define two functions: the first which returns the 
;;  estimate of teh travel time, the second which returns the estimate of 
;;  the cost of travel

(defun f-h-time (state sensors)
  (if (null sensors)
    nil
    (if (equal state (first (first sensors)))
      (first (second (first sensors)))
      (f-h-time state (rest sensors)))))  
    

(defun f-h-price (state sensors)
    (if (null sensors)
    nil
    (if (equal state (first (first sensors)))
      (second (second (first sensors)))
      (f-h-price state (rest sensors)))))
;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General navigation function
;;
;;  Returns the actions that can be carried out from the current state
;;
;;  Input:
;;    state:      the state from which we want to perform the action 
;;    lst-edges:  list of edges of the graph, each element is of the 
;;                form: (source destination (cost1 cost2))
;;    c-fun:      function that extracts the correct cost (time or price)
;;                from the pair that appears in the edge
;;    name:       name to be given to the actions that are created (see the 
;;                action structure)
;;    forbidden-cities:  
;;                list of the cities where we can't arrive by train
;;
;;  Returns
;;    A list of action structures with the origin in the current state and
;;    the destination in the states to which the current one is connected
;;
(defun navigate (state lst-edges cfun name &optional forbidden)
  ; Comprobamos si el grafo que nos pasan esta vacio
  (if (null lst-edges)
    nil
    ; Comprobamos si la ciudad de origen coincide con la ciudad de origen del vertice del grafo
    (if (equal (first (first lst-edges)) state)
      ; Comprobamos si es una ciudad de la lista de prohibidas
      (if (null (member (second (first lst-edges)) forbidden))
        ; Obtenemos el coste de la accion para posteriormente crear la estructura
        (let ((valor (funcall cfun (first lst-edges))))
          ; Definimos ahora la estructura accion
              (let ((accion (make-action :name name
                                         :origin state
                                         :final (second (first lst-edges))
                                         :cost valor)))
              ; Devolvemos la estructura y hacemos la llamada recursiva
              (cons accion (navigate state (rest lst-edges) cfun name forbidden))))
        ; Si esta en la lista de prohibidas continuamos testando
        (navigate state (rest lst-edges) cfun name forbidden))

      ; Si la ciudad de origen no es la del vertice del grafo seguimos comparando
      (navigate state (rest lst-edges) cfun name forbidden))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by canal
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of canals, returns a list of actions to navigate
;; from the current city to the cities reachable from it by canal navigation.
;;
(defun navigate-canal-time (state canals)
  ; Llamamos a la funcion de navegacion que hemos codicado anteriormente con la funcion correcta
  (navigate state canals #'(lambda(x) (first (third x))) "NAVIGATE CANAL TIME")) 

(defun navigate-canal-price (state canals)
  (navigate state canals #'(lambda(x) (second (third x))) "NAVIGATE CANAL PRICE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation by train
;;
;; This is a specialization of the general navigation function: given a 
;; state and a list of train lines, returns a list of actions to navigate
;; from the current city to the cities reachable from it by train.
;; 
;; Note that this function takes as a parameter a list of forbidden cities.
;;
(defun navigate-train-time (state trains forbidden)
  (navigate state trains #'(lambda(x) (first (third x))) "NAVIGATE TRAIN TIME" forbidden))
  
(defun navigate-train-price (state trains forbidden)
  (navigate state trains #'(lambda(x) (second (third x))) "NAVIGATE TRAIN PRICE" forbidden))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal test
;;
;;  Returns T or NIl depending on whether a path leads to a final state
;;
;;  Input:
;;    node:       node structure that contains, in the chain of parent-nodes,
;;                a path starting at the initial state
;;    destinations: list with the names of the destination cities
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the path is a valid path to the final state
;;    NIL: invalid path: either the final city is not a destination or some
;;         of the mandatory cities are missing from the path.
;;
(defun f-goal-test (node destination mandatory) 
  ; Creamos una lista que contenga todas las ciudades del recorrido
  (let ((ciudades (get-path node)))
    ; Utilizamos la funcion auxiliar para comprobar si se cumplen las condiciones
    (check-aux ciudades destination mandatory)))
    


; Funcion auxiliar que devuelve una lista de ciudades dado un nodo
(defun get-path(node)
  (if (null node)
    nil
    (cons (node-state node) (get-path (node-parent node)))))

; Funcion auxiliar que comprueba si dado un camino se ha pasado por todas las ciudades obligadas
; y al menos por una destinos
(defun check-aux (ciudades destination mandatory)
  ; Comprobamos si se han checkeado todas las ciudades obligadas
  (if (null mandatory)
    ; Comprobamos si se ha llegado al final de las ciudades destino
    (if (null destination)
      nil
      ; Si no se ha llegado al final comprobamos si pasa por la siguiente ciudad destino
      (if (null (member (first destination) ciudades))
        ; Si no pasa por la ciudad seguimos comprobando
        (check-aux ciudades (rest destination) mandatory)
        ; Si pasa por la ciudad devolvemos true
        t))
    ; Si aun quedan ciudades obligatorias comprobamos
    (if (null (member (first mandatory) ciudades))
      ; Si no se encuentra en la lista de ciudades devolvemos nil
      nil
      ; Si se encuentra en la lista seguimos comprobando
      (check-aux ciudades destination (rest mandatory)))))

;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 4 -- Equal predicate for search states
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Determines if two nodes are equivalent with respect to the solution
;; of the problem: two nodes are equivalent if they represent the same city 
;, and if the path they contain includes the same mandatory cities.
;;  Input:
;;    node-1, node-2: the two nodes that we are comparing, each one
;;                    defining a path through the parent links
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the two ndoes are equivalent
;;    NIL: The nodes are not equivalent
;;
(defun f-search-state-equal (node-1 node-2 &optional mandatory)
  ; Primero comprobamos si la ciudad de los dos nodos es la misma
  (if (equal (node-state node-1) (node-state node-2))
    ; Si se cumple comprobamos si el primer nodo pasa por las ciudades obligadas, le pasaremos el propio nodo como ciudad
    ; destino para poder usar la funcion del ejercicio anterior
    (if (null (f-goal-test node-1 (list (node-state node-1)) mandatory))
      ; Si es null devolvemos nil
      nil
      ; Si no es null pasamos a comprobar el nodo 2
      (f-goal-test node-2 (list (node-state node-2)) mandatory))
    ; Si la ciudad de los nodos no es la misma devolvemos nil
    nil))

;;
;; END: Exercise 4 -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 5 -- Define the problem structure
;;
;;
;;  Note that the connectivity of the netowrk using canals and trains
;;  holes is implicit in the operators: there is a list of two
;;  operators, each one takes a single parameter: a state name, and
;;  returns a list of actions, indicating to which states one can move
;;  and at which cost. The lists of edges are placed as constants as
;;  the second parameter of the navigate operators. 
;;
;;  There are two problems defined: one minimizes the travel time,
;;  the other minimizes the cost

(defparameter *travel-cheap*
  (make-problem 
    :states *cities*
    :initial-state *origin* 
    :f-h #'(lambda(state) (f-h-price state  *estimate*))
    :f-goal-test #'(lambda(node) (f-goal-test node *destination* *mandatory*))
    :f-search-state-equal #'(lambda (node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))
    :operators (list #'(lambda(node) (navigate-train-price (node-state node) *trains* *forbidden*)) #'(lambda(node) (navigate-canal-price (node-state node) *canals*)))))

(defparameter *travel-fast*
  (make-problem
    :states *cities*
    :initial-state *origin* 
    :f-h #'(lambda(state) (f-h-time state  *estimate*))
    :f-goal-test #'(lambda(node) (f-goal-test node *destination* *mandatory*))
    :f-search-state-equal #'(lambda(node-1 node-2) (f-search-state-equal node-1 node-2 *mandatory*))
    :operators (list #'(lambda(node) (navigate-train-time (node-state node) *trains* *forbidden*)) #'(lambda(node) (navigate-canal-time (node-state node) *canals*)))))
;;
;;  END: Exercise 5 -- Define the problem structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 6: Expand node
;;
;; The main function of this section is "expand-node", which receives
;; a node structure (the node to be expanded) and a problem structure.
;; The problem structure has a list of navigation operators, and we
;; are interested in the states that can be reached using any one of
;; them.
;;
;; So, in the expand-node function, we iterate (using mapcar) on all
;; the operators of the problem and, for each one of them, we call
;; expand-node-operator, to determine the states that can be reached
;; using that operator.
;;
;; The operator gives us back a list of actions. We iterate again on
;; this list of action and, for each one, we call expand-node-action
;; that creates a node structure with the node that can be reached
;; using that action.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Creates a list with all the nodes that can be reached from the
;;  current one using all the operators in a given problem
;;
;;  Input:
;;    node:   the node structure from which we start.
;;    problem: the problem structure with the list of operators
;;
;;  Returns:
;;    A list (node_1,...,node_n) of nodes that can be reached from the
;;    given one
;;
(defun expand-node (node problem)
  ; Lambda que crea nodo despues de expandirlo utilizando la funcion de problem
  (mapcar #'(lambda(accion) (make-node :state (action-final accion)
                                     :parent node
                                     :action accion
                                     :depth (+ (node-depth node) 1)
                                     :g (+ (node-g node) (action-cost accion))
                                     :h (funcall (problem-f-h problem) (action-final accion))
                                     :f (+ (+ (node-g node) (action-cost accion))
                                              (let ((heuristica (funcall (problem-f-h problem) (action-final accion))))
                                                (if (null heuristica)
                                                  0
                                                  heuristica)))))
                              (append (funcall (first (problem-operators problem)) node) (funcall (second (problem-operators problem)) node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 7 -- Node list management
;;;  
;;;  Merges two lists of nodes, one of them ordered with respect to a
;;;  given strategy, keeping the result ordered with respect to the
;;;  same strategy.
;;;
;;;  This is the idea: suppose that the ordering is simply the
;;;  ordering of natural numbers. We have a "base" list that is
;;;  already ordered, for example:
;;;      lst1 --> '(3 6 8 10 13 15)
;;;
;;;  and a list that is not necessarily ordered:
;;;
;;;      nord --> '(11 5 9 16)
;;;
;;;  the call (insert-nodes nord lst1 #'<) would produce
;;;
;;;    (3 5 6 8 9 10 11 13 15 16)
;;;
;;;  The functionality is divided in three functions. The first,
;;;  insert-node, inserts a node in a list keeping it ordered. The
;;;  second, insert-nodes, insert the nodes of the non-ordered list
;;;  into the ordered, one by one, so that the two lists are merged.
;;;  The last function, insert-node-strategy is a simple interface that
;;;  receives a strategy, extracts from it the comparison function, 
;;;  and calls insert-nodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    node-compare-p: a function node x node --> 2 that returns T if the 
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;   criterion node-compare-p.
;; 
(defun insert-nodes (nodes lst-nodes node-compare-p)
  ; Usamos sort y append para crear la lista de nodos ordenados
  (sort (append nodes lst-nodes) node-compare-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect the given strategy
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    strategy: the strategy that gives the criterion for node
;;              comparison
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;    criterion defined in te strategy.
;; 
;; Note:
;;   You will note that this function is just an interface to
;;   insert-nodes: it allows to call using teh strategy as a
;;   parameter; all it does is to "extract" the compare function and
;;   use it to call insert-nodes.
;;
(defun insert-nodes-strategy (nodes lst-nodes strategy)
  ; Igual que en el ejercicio anterior usamos sort y append para generar la lista de nodos ordenada
  (sort (append nodes lst-nodes) (strategy-node-compare-p strategy)))

;;
;;    END: Exercize 7 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 8 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;
; Funcion que evalua dos nodos utilizando la comparacion A*
(defun node-f-<= (node-1 node-2)
  ; Evaluacion de los nodos en funcion del valor de f de los mismos, para realizar la busqueda A*
  (or (< (node-f node-1) (node-f node-2)) (and (=(node-f node-1)(node-f node-2)) (<= (node-depth node-1)(node-depth node-2)))))


(defparameter *A-star*
  (make-strategy 
    :name "A-star"
    :node-compare-p 'node-f-<=))

;;
;; END: Exercise 8 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Search algorithm
;;;
;;;    Searches a path that solves a given problem using a given search
;;;    strategy. Here too we have two functions: one is a simple
;;;    interface that extracts the relevant information from the
;;;    problem and strategy structure, builds an initial open-nodes
;;;    list (which contains only the starting node defined by the
;;;    state), and initial closed node list (the empty list), and calls
;;;    the auxiliary function.
;;;
;;;    The auxiliary is a recursive function that extracts nodes from
;;;    the open list, expands them, inserts the neighbors in the
;;;    open-list, and the expanded node in the closed list. There is a
;;;    caveat: with this version of the algorithm, a node can be
;;;    inserted in the open list more than once. In this case, if we
;;;    extract a node in the open list and the following two condition old:
;;;
;;;     the node we extract is already in the closed list (it has
;;;     already been expanded)
;;;       and
;;;     the path estimation that we have is better than the one we
;;;     obtain from the node in the open list
;;;
;;;     then we ignore the node.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auxiliary search function (the one that actually does all the work
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos, etc.
;;    open-nodes: the list of open nodes, nodes that are waiting to be
;;                visited
;;    closed-nodes: the list of closed nodes: nodes that have already
;;                  been visited
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;     Note that what is returned is quite a complex structure: the
;;     node contains in "parent" the node that comes before in the
;;     path, that contains another one in "parents" and so on until
;;     the initial one. So, what we have here is a rather complex
;;     nested structure that contains not only the final node but the
;;     whole path from the starting node to the final.
;;
(defun graph-search-aux (problem open-nodes closed-nodes strategy)
  ; Usamos cond para evaluar todas las posibles condiciones
  (cond
   ; Si la lista de nodos esta vacia devolvemos nil
   ((null open-nodes)
      nil)
   ; Si quedan nodos en la lista de abiertos pasamos a evaluar
   ; Primero comprobamos si el primer nodo abierto satiface la condicion objetivo
   ((funcall (problem-f-goal-test problem) (first open-nodes)) 
      ; Si la satisface lo devolvemos
      (first open-nodes))
   ; Si el primer nodo no satisface la condicion objetivo pasamos a expandirlo y seguir evaluando
   ((or (not (member (first open-nodes) closed-nodes))
        (funcall (strategy-node-compare-p strategy) (first open-nodes) (first (member (first open-nodes) closed-nodes))))
            (graph-search-aux problem
                 (insert-nodes-strategy  (expand-node (first open-nodes) problem) (rest open-nodes) strategy)
                 (cons (first open-nodes) closed-nodes)
                 strategy))

   ; Si no estamos ante niguno de los casos seguimos con la exploracion quitando el primer nodo de la lista de abiertos
   (t (graph-search-aux problem (rest open-nodes) closed-nodes strategy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface function for the graph search. 
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos,
;;             starting node, heuristic, etc.
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;    See the graph-search-aux for the complete structure of the
;;    returned node. 
;;    This function simply prepares the data for the auxiliary
;;    function: creates an open list with a single node (the source)
;;    and an empty closed list.
;;
(defun graph-search (problem strategy)
  ; Llamamos a la funcion auxiliar creando un nodo nuevo que comience en el estado inicial del problema
  (graph-search-aux problem (list (make-node :state (problem-initial-state problem)
                                             :depth 0
                                             :g 0
                                             :f 0))
                            ; Pasamos una lista vacia como lista de nodos cerrados, esta lista se ira rellenando 
                            ; en el proceso de exploracion
                            (list )
                            strategy))

;
;  A* search is simply a function that solves a problem using the A* strategy
;
(defun a-star-search (problem)
  ; Llamamos a la funcion de busqueda con la estrategia A*
  (graph-search problem *A-star*))

;;
;; END: Exercise 9 -- Search algorithm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: Solution path
;;;
;*** solution-path ***

(defun solution-path (node)
  ; Si el nodo es igual a null devolvemos nil
  (if (null node)
    nil
    ; Si no es igual a null pasamos a imprimir el camino
    (solution-path-aux node ())))

(defun solution-path-aux (node path)
  ; Comprobamos si existe tanto nodo hijo como padre
  (if (and node (node-parent node))
    ; Si existen pasamos a llamar a la funcion a llamar de nuevo a la fucion para seguir guardando nodos en el path
    (solution-path-aux (node-parent node) (cons (node-state node) path))
    ; Si hemos acabado de evaluar los nodos padre pasamos a devolver el path
    (cons (node-state node) path)))

;*** action-sequence ***
; Visualize sequence of actions

(defun action-sequence (node)
  ; Misma comprobacion que en el caso anterior
  (if (null node)
    ; Si el nodo esta vacio devolvemos nil
    nil
    ; Si no es nil pasamos a imprimir la secuencia de acciones
    (action-sequence-aux node ())))

(defun action-sequence-aux(node path)
  ; Comprobamos si hemos acabado de evaluar el camino
  ; Si existe el nodo y la accion seguimos creando el path
  (if (and node (node-action node))
    ; Si se puede seguir creando el camino hacemos la recursion con el nodo padre
    (action-sequence-aux (node-parent node) (cons (node-action node) path))
    ; Si hemos acdabado de evaluar devolvemos el camino con las acciones
    path))

;;; 
;;;    END Exercise 10: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Busqueda en profundidad
(defparameter *depth-first*
  ; Creamos la estrategia que utiliza la funcion especificada
  (make-strategy
   :name "DEPTH FIRST SEARCH"
   :node-compare-p 'depth-first-node-compare-p))

; Comparacion para busqueda en profundidad
(defun depth-first-node-compare-p (node-1 node-2)
  (> (node-depth node-1) (node-depth node-2)))

; Busqueda en anchura
(defparameter *breadth-first*
  ; Creamos la estrategia que utiliza la funcion especificada
  (make-strategy
   :name "BREADDTH FIRST SEARCH"
   :node-compare-p 'breadth-first-node-compare-p))

; Comparacion para busqueda en anchura
(defun breadth-first-node-compare-p (node-1 node-2)
  (< (node-depth node-1) (node-depth node-2)))
