%Ejercicio 1
%Caso base del predicado duplica en el que se nos dan dos listas vacias
duplica([],[]).
%Caso recursivo en el que se comprueba si el primer elemento de la lista 1 se encuentra duplicado
%en la lista 2. Si es asi continuamos con la recursion.
duplica([X|Ys], [X,X|Zs]) :- duplica(Ys, Zs).

%Ejercicio 2
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

%Caso base del predicado invierte en el que nos pasan una lista vacia
invierte([], []).
%Caso recursivo en el que haciendo uso de la funcion concatena comprobamos si el primer elemento de la lista
%se encuentra concatenado por la parte de atras de la segunda lista, es decir, se encuentra invertido
invierte([X|Xs], R):- invierte(Xs, Z),
                      concatena(Z, [X], R).

%Ejercicio 3
%Caso base en el que se nos da una lista vacia
palindromo([]).
%Comprobamos si la lista que se nos da es igual a la misma invertida
palindromo(X) :- invierte(X, X).

%Ejercicio 4
%Caso base en el que se nos pasan listas vacias
divide([],0,[],[]).
%Predicado en el que se fragmenta la lista L guardando en L1 los N primeros elementos de la L y en L2 el resto
%Para completar este predicado hacemos uso de concatena comprobando si el resultado obtenido en la fragmentacion es igual a L
divide(L,N,L1,L2) :- concatena(L1, L2, L),
             length(L1, N).

%Ejercicio 5
%Caso base en el que tenemos una lista vacia
aplasta([],[]).

%Caso en el que el primer elemento de la lista es otra lista
%Para resolver este caso hacemos una recursion con el primer elemento de la lista y con el resto de elementos
%Una vez hemos obtenido los resultados de la recursiones anteriores las concatenamos en L3
aplasta([X|Xs], L3) :- is_list(X),
               aplasta(X, L2),
               aplasta(Xs, L1),
               concatena(L2, L1, L3).

%Si el primer elemento no es una lista lo guardamos directamente y llamamos recursivamenete al predicado aplasta con el resto de elementos
aplasta([X|Xs], [X|L1]) :- not(is_list(X)),
                 aplasta(Xs, L1).

%Ejercicio 6
%Predicado que devuelve todos los elementos que resultan de la factorizacion de un numero
%Hacemos uso de la funcion auxiliar que nos devuelve todos los factores de un numero
primos(N, L) :-
    next_factor(N, 2, L).

%Caso base de la funcion next_factor en el que ya no quedan mas factores
next_factor(N, F, F) :- 0 is N mod F. 

%Caso recursivo en el que se comprueba si el factor es menor que el numero, si el resto del mismo es 0
%Si el resto del mismo es 0 se llama recursivamente a next_factor con el resultado de la division del numero
%Si e modulo no es 0 se siguen comprobando factores
next_factor(N, F, NF) :- F < N,
               (0 is N mod F ->  
                          (N1 is N/F, next_factor(N1, F, NF));
                          (F1 is F + 1, next_factor(N, F1, NF))).
              

%Ejercicio 7.1
%Caso base en el que el ultimo elemento de la lista es igual al elemento que buscamos
cod_primero(X, [], [], [X]).
%Recursion en la que se guarda el elemento en la lista Rfront si este es igual al elemento que buscamos
%Si el elemento es igual se seguen comprobando el resto de elementos
cod_primero(X, [X|Xs], Lrem, [X|Ys]) :- cod_primero(X, Xs, Lrem, Ys).
%Si el elemento que buscamos es diferent lo guardamos en la lista y almacenamos el resto de elementos en Lrem
cod_primero(X, [Y|Ys], [Y|Ys], [X]) :- X \== Y.

%Ejercicio 7.2
%Caso base de la recursion
cod_all([],[]).
%Hacemos la recursion buscando los elementos cod_primero del primer elemento sin quitarlo de la lista en la que buscamos
%Llamamos recurivamente a cod_all con el resto de elementos de Lrem para seguir evaluando
cod_all([X|Xs], [Fs|Ys]) :- cod_primero(X, [X|Xs], Z, [X|Fs]),
                 cod_all(Z, Ys).

%Ejercicio 7.3
%Caso base en el que se nos da una lista vacia
run_length([],[]).
%Primero se hace uso del predicado cod_all para obtener la lista que a continuacion pasaremos al predicado contador_elementos que nos la
%devolvera en el formato correcto
run_length(L, L1) :- cod_all(L, L2),
             contador_elementos(L2, L1).

%Caso base de la funcion que dada una lista del tipo cod_all guarda la longitud de cada elemento
%de la lista asi como el primer elemento
%En este caso base se evalua el final de la cuenta
contador_elementos([],[]).
%Funcion recursiva que guarda la longitud del primer elemento de la lista asi como su first
%Llama recursivamente a la funcion con el resto de elementos
contador_elementos([X|Xs], [[Y,Z]|R]) :- length(X, Y),
                                         first(X, Z),
                                         contador_elementos(Xs, R).

%Funcion auxiliar que utilizamos para obtener el primer elemento de una lista
%Caso base en el que se envia una lista vacia
first([],[]).
%Caso en el que devolvemos el primer elemento de la lista
first([X|_],X).