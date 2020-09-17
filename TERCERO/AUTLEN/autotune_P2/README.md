# AutoTune

#TODO::
    + AFNDCierraLTransicion(p_afnd_l);
    
        Crea transiciones necesarias:
            Q1 -(lambda)-> Q2
            Q2 -(lambda)-> Q3
            Tenemos tambien ==> Q1 -(lambda)-> Q2 -(lambda)-> Q3
            
        Se realiza con 3 bucles:: K, i, j;
        Se puede hacer con una matriz solo
        Operacion relevante:
            W(K+1)[i][j] = W(K)[i][j] OR ( W(K)[i][k] AND W(K)[k][j] )
        
    + Cercionarse de que los simbolos van bien:
    
        - Tiene que haber maxSimbolos + 1
        - if(alfabeto->last_elemento == alfabeto->tamanio - 1) No podemos insertar
        
        Identificar si hay más de este tipo.
        
    + Mirar inicializaActual():
    
        - Ahora tendremos que mirar transiciones lambda en el nodo INICIAL
        - Flag por si no hay estado INICIAL
    
#HECHO::
    + AFNDInsertaLTransicion
    
        Inserta una transicion labmda donde especifique.
            
    + p_afnd_l = AFNDInicializaCadenaActual(p_afnd_l);
    
        Elimina la cadena actual entera, para insertar otra nueva.
