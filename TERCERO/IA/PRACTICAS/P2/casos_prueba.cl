;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests for the required functions in lab 2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 1:
;;
;;

(f-h-time 'Nantes *estimate*) ; -> 75.0
(f-h-time 'Marseille *estimate*) ; -> 145.0
(f-h-time 'Lyon *estimate*) ; -> 105.0
(f-h-time 'Madrid *estimate*) ; -> NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 2:
;;
;;

(navigate-canal-time 'Avignon *canals*) ;->
;(#S(ACTION :NAME NAVIGATE-CANAL-TIME
;           :ORIGIN AVIGNON
;           :FINAL MARSEILLE
;           :COST 35.0))

(navigate-train-price 'Avignon *trains* '()) ;->
;(#S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN AVIGNON
;           :FINAL LYON
;           :COST 40.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN AVIGNON
;           :FINAL MARSEILLE
;           :COST 25.0))


(navigate-train-price 'Avignon *trains* '(Marseille)) ;->
;(#S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN AVIGNON
;           :FINAL LYON
;           :COST 40.0))

(navigate-canal-time 'Orleans *canals*)  ;-> NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 3:
;;
;;


(defparameter node-nevers
   (make-node :state 'Nevers) )
(defparameter node-paris
   (make-node :state 'Paris :parent node-nevers))
(defparameter node-nancy
   (make-node :state 'Nancy :parent node-paris))
(defparameter node-reims
   (make-node :state 'Reims :parent node-nancy))
(defparameter node-calais
   (make-node :state 'Calais :parent node-reims))


(f-goal-test node-calais '(Calais Marseille) '(Paris Limoges)); -> NIL
(f-goal-test node-paris '(Calais Marseille) '(Paris)); -> NIL
(f-goal-test node-calais '(Calais Marseille) '(Paris Nancy)); -> T

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 4:
;;
;;

(defparameter node-calais-2
   (make-node :state 'Calais :parent node-paris))

(f-search-state-equal node-calais node-calais-2 '()) ;-> T
(f-search-state-equal node-calais node-calais-2 '(Reims)) ;-> NIL
(f-search-state-equal node-calais node-calais-2 '(Nevers)) ;-> T
(f-search-state-equal node-nancy node-paris '()) ;-> NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 6:
;;
;; NOTE: In order to run the tests from this point on, you must 
;; have solved exercise 5, that is, you must have created the 
;; structures *travel-ceap* and *travel-fast*
;;
;; ALSO NOTE: The output of tehse examples is "as is", the same as
;; it appears on the allegro console. The console "cuts" some of 
;; the outputs when these are too complex. Your output might be 
;; slightly diffferent than this one.

(defparameter node-marseille-ex6
   (make-node :state 'Marseille :depth 12 :g 10 :f 20) )

(defparameter lst-nodes-ex6
  (expand-node node-marseille-ex6 *travel-fast*)) 

(print lst-nodes-ex6) ; ->
;(#S(NODE :STATE TOULOUSE
;         :PARENT #S(NODE
;                    :STATE
;                    MARSEILLE
;                    :PARENT
;                    NIL
;                    :ACTION
;                    NIL
;                    :DEPTH
;                    12
;                    :G
;                    10
;                    :H
;                    0
;                    :F
;                    20)
;         :ACTION #S(ACTION
;                    :NAME
;                    NAVIGATE-TRAIN-TIME
;                    :ORIGIN
;                    MARSEILLE
;                    :FINAL
;                    TOULOUSE
;                    :COST
;                    65.0)
;         :DEPTH 13
;        :G 75.0
;         :H 130.0
;         :F 205.0)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 7:
;;

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))

(defparameter node-paris-ex7
  (make-node :state 'Paris :depth 0 :g 0 :f 0) )

(defparameter node-nancy-ex7
  (make-node :state 'Nancy :depth 2 :g 50 :f 50) )


(defparameter sol-ex7 (insert-nodes-strategy (list node-paris-ex7 node-nancy-ex7) 
                                             lst-nodes-ex6
                                             *uniform-cost*))

(mapcar #'(lambda (x) (node-state x)) sol-ex7) ; -> (PARIS NANCY TOULOUSE)
(mapcar #'(lambda (x) (node-g x)) sol-ex7) ; -> (0 50 75)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 8:
;;

(graph-search *travel-cheap* *A-star*);->
;#S(NODE :STATE CALAIS
;        :PARENT #S(NODE
;                   :STATE
;                   REIMS
;                   :PARENT
;                   #S(NODE
;                      :STATE
;                      PARIS
;                     :PARENT
;                     #S(NODE
;                         :STATE
;                         NEVERS
;                        :PARENT
;                        #S(NODE
;                           :STATE
;                            LIMOGES
;                            :PARENT
;                            #
;                            :ACTION
;                            #
;                            :DEPTH
;                            2
;                            :G
;                            ...)
;                         :ACTION
;                        #S(ACTION
;                            :NAME
;                           NAVIGATE-TRAIN-PRICE
;                            :ORIGIN
;                            LIMOGES
;                            :FINAL
;                            NEVERS
;                            :COST
;                            60.0)
;                         :DEPTH
;                         3
;                         :G
;                        ...)
;                      :ACTION
;                      #S(ACTION
;                        :NAME
;                         NAVIGATE-CANAL-PRICE
;                         :ORIGIN
;                         NEVERS
;                         :FINAL
;                         PARIS
;                         :COST
;                         10.0)
;                      :DEPTH
;                      4
;                      :G
;                      ...)
;                   :ACTION
;                   #S(ACTION
;                      :NAME
;                      NAVIGATE-CANAL-PRICE
;                      :ORIGIN
;                      PARIS
;                      :FINAL
;                      REIMS
;                      :COST
;                      10.0)
;                   :DEPTH
;                   5
;                   :G
;                   ...)
;        :ACTION #S(ACTION
;                   :NAME
;                   NAVIGATE-CANAL-PRICE
;                   :ORIGIN
;                   REIMS
;                   :FINAL
;                   CALAIS
;                   :COST
;                   15.0)
;        :DEPTH 6
;        :G ...)


(a-star-search *travel-fast*) ;->
;#S(NODE :STATE CALAIS
;        :PARENT #S(NODE
;                   :STATE
;                   PARIS
;                   :PARENT
;                   #S(NODE
;                      :STATE
;                      ORLEANS
;                      :PARENT
;                      #S(NODE
;                         :STATE
;                         LIMOGES
;                         :PARENT
;                         #S(NODE
;                            :STATE
;                            TOULOUSE
;                            :PARENT
;                            #
;                            :ACTION
;                            #
;                            :DEPTH
;                            1
;                            :G
;                            ...)
;                         :ACTION
;                         #S(ACTION
;                            :NAME
;                            NAVIGATE-TRAIN-TIME
;                            :ORIGIN
;                            TOULOUSE
;                            :FINAL
;                            LIMOGES
;                            :COST
;                            25.0)
;                         :DEPTH
;                         2
;                         :G
;                         ...)
;                      :ACTION
;                      #S(ACTION
;                         :NAME
;                         NAVIGATE-TRAIN-TIME
;                         :ORIGIN
;                         LIMOGES
;                         :FINAL
;                         ORLEANS
;                         :COST
;                         55.0)
;                      :DEPTH
;                      3
;                      :G
;                      ...)
;                   :ACTION
;                   #S(ACTION
;                      :NAME
;                      NAVIGATE-TRAIN-TIME
;                      :ORIGIN
;                      ORLEANS
;                      :FINAL
;                      PARIS
;                      :COST
;                      23.0)
;                   :DEPTH
;                   4
;                   :G
;                   ...)
;        :ACTION #S(ACTION
;                   :NAME
;                   NAVIGATE-TRAIN-TIME
;                   :ORIGIN
;                   PARIS
;                   :FINAL
;                   CALAIS
;                   :COST
;                   34.0)
;        :DEPTH 5
;       :G ...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Exercise 9:
;;


(solution-path nil) ;-> NIL

(solution-path (a-star-search *travel-fast*)) ; ->
; (MARSEILLE TOULOUSE LIMOGES ORLEANS PARIS CALAIS)

(solution-path (a-star-search *travel-cheap*))  ; ->
; (MARSEILLE TOULOUSE LIMOGES NEVERS PARIS REIMS CALAIS)

(action-sequence (a-star-search *travel-fast*)) ; ->
;(#S(ACTION :NAME NAVIGATE-TRAIN-TIME
;           :ORIGIN MARSEILLE
;           :FINAL TOULOUSE
;           :COST 65.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-TIME
;           :ORIGIN TOULOUSE
;           :FINAL LIMOGES
;           :COST 25.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-TIME
;           :ORIGIN LIMOGES
;           :FINAL ORLEANS
;           :COST 55.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-TIME
;           :ORIGIN ORLEANS
;           :FINAL PARIS
;           :COST 23.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-TIME
;           :ORIGIN PARIS
;           :FINAL CALAIS
;           :COST 34.0))

(action-sequence (a-star-search *travel-cheap*)) ; ->
;(#S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN MARSEILLE
;           :FINAL TOULOUSE
;           :COST 120.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN TOULOUSE
;           :FINAL LIMOGES
;           :COST 35.0)
; #S(ACTION :NAME NAVIGATE-TRAIN-PRICE
;           :ORIGIN LIMOGES
;           :FINAL NEVERS
;           :COST 60.0)
; #S(ACTION :NAME NAVIGATE-CANAL-PRICE
;           :ORIGIN NEVERS
;           :FINAL PARIS
;           :COST 10.0)
; #S(ACTION :NAME NAVIGATE-CANAL-PRICE
;           :ORIGIN PARIS
;           :FINAL REIMS
;           :COST 10.0)
; #S(ACTION :NAME NAVIGATE-CANAL-PRICE
;           :ORIGIN REIMS
;           :FINAL CALAIS
;           :COST 15.0))