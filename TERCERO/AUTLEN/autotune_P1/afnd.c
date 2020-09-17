#include "afnd.h"
#include "alfabeto.h"
#include "estado.h"
#include "palabra.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*COSAS QUE FALTAN POR TERMINAR:
    - PROCESA ENTRADA
*/

/*FUNCION PRIVADA QUE DEVUELVE UN ARRAY DE ESTADOS ACTUALES A 0*/
BOOL* inicializaActual(int maxEstados);

BOOL* inicializaActual(int maxEstados){
    BOOL* vector;
    int i;
    
    if(maxEstados <= 0)
        return NULL;
        
    vector = (BOOL*)malloc(sizeof(BOOL) * maxEstados);
    for(i = 0; i < maxEstados; i++)
        vector[i] = FALSE;
    
    return vector;
}

struct _AFND{
    char* nombre;
    int maxEstados;
    int maxSimbolos;
    int countEstadosActuales;
    Alfabeto* simbolos;
    Estado** estados;
    Palabra* palabra;
    BOOL*** transiciones;
    BOOL* actuales;
};

AFND* AFNDNuevo(char* nombre, int numEstados, int numSimbolos){
    AFND* newAFND = NULL;
    int i, j, k;
    
    if (nombre == NULL || numEstados <= 0 || numSimbolos <= 0)
        return NULL;
    
    newAFND = (AFND*) malloc(sizeof(AFND));
    
    newAFND->nombre = (char*) malloc(strlen(nombre) * sizeof(char) + 1);
    strcpy(newAFND->nombre, nombre);
    newAFND->maxEstados = numEstados;
    newAFND->maxSimbolos = numSimbolos;
    newAFND->countEstadosActuales = 0;
    newAFND->simbolos = crearAlfabeto(numSimbolos);
    
    newAFND->estados = (Estado**) malloc(sizeof(Estado*) * numEstados);
    for (i = 0; i < numEstados; i++){
        newAFND->estados[i] = NULL;
    }
        
    newAFND->palabra = crearPalabra();
    
    newAFND->transiciones = (BOOL***) malloc(sizeof(BOOL**) * newAFND->maxEstados);
    for (i = 0; i < newAFND->maxEstados; i++){
        newAFND->transiciones[i] = (BOOL**) malloc(sizeof(BOOL*) * newAFND->maxSimbolos);
        for (j = 0; j < newAFND->maxSimbolos; j++){
            newAFND->transiciones[i][j] = (BOOL*) malloc(sizeof(BOOL) * newAFND->maxEstados);
        }
    }
    for (i = 0; i < newAFND->maxEstados; i++){
        for (j = 0; j < newAFND->maxSimbolos; j++){
            for (k = 0; k < newAFND->maxEstados; k++)
                newAFND->transiciones[i][j][k] = FALSE;
        }
    }
    
    newAFND->actuales = (BOOL*) malloc(sizeof(BOOL) * newAFND->maxEstados);
    for (i = 0; i < newAFND->maxEstados; i++)
        newAFND->actuales[i] = FALSE;
    
    return newAFND;
}

BOOL AFNDInsertaSimbolo(AFND* afnd, char* simbolo){
    
    char* elementoInsertado = NULL; 
    
    if(afnd == NULL || simbolo == NULL)
        return FALSE;
    
    elementoInsertado = insertarElemento(afnd->simbolos, simbolo);
    if (elementoInsertado == NULL){
    printf(">> El simbolo (%s) no se puede insertar debido a que superamos el limite!\n", simbolo);
        return FALSE;
    }
    return TRUE;
}

BOOL AFNDInsertaEstado(AFND* afnd, char* nombre, ESTADO estado){
    
    Estado* newEstado = NULL;
    int i;
    
    if (afnd == NULL || nombre == NULL)
        return FALSE;
    newEstado = crearEstado(nombre, estado);
    for (i = 0; i < afnd->maxEstados; i++) {
        if (afnd->estados[i] == NULL){
            afnd->estados[i] = newEstado;
            return TRUE;
        }
    }
    
    destruirEstado(newEstado);
    printf(">> El estado (%s) no se puede insertar debido a que superamos el limite!\n", nombre);
    return FALSE;
}

BOOL AFNDInsertaTransicion(AFND* afnd, char* estado1, char* simbolo, char* estado2){
    
    int indexEstI, indexElemento, indexEstF;
    
    if(!afnd || !estado1 || !simbolo || !estado2)
        return FALSE;
    
    indexEstI = get_indiceEstado(afnd, estado1);
    if (indexEstI == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado1);
        return FALSE;
    }
    
    indexElemento = buscarElemento(afnd->simbolos, simbolo);
    if (indexElemento == -1){
        printf(">> El simbolo (%s) no esta reconocido en nuestro alfabeto!\n", simbolo);
        return FALSE;
    }
    
    indexEstF = get_indiceEstado(afnd, estado2);
    if (indexEstF == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado2);
        return FALSE;
    }
    
    afnd->transiciones[indexEstI][indexElemento][indexEstF] = TRUE;
    
    return TRUE;
}

void AFNDImprime(FILE* pf, AFND* afnd){
    int i, j, k;
    
    if (pf == NULL || afnd == NULL)
        return;
        
    fprintf(pf, "%s={\n\tnum_simbolos = %d\n\n\t", afnd->nombre, alfabeto_size(afnd->simbolos));
    imprimeAlfabeto(pf, afnd->simbolos);
    fprintf(pf, "\n\n\tnum_estados = %d", afnd->maxEstados);
    fprintf(pf, "\n\n\tQ = {");
    for(i = 0; i < afnd->maxEstados; i++){
        if(afnd->estados[i] != NULL){
            imprimeEstado(pf, afnd->estados[i]);
            fprintf(pf, " ");
        }
        else
            break;
    }
    fprintf(pf, "}\n\n\t");
    fprintf(pf, "Funcion de transicion = {\n\n");
    for(i = 0; i < afnd->maxEstados; i++){
        for(j = 0; j < afnd->maxSimbolos; j++){
            fprintf(pf, "\t\tf(%s, %s) = { ", estado_getNombre(afnd->estados[i]), alfabeto_get_i(afnd->simbolos, j));
            for (k = 0; k < afnd->maxEstados; k++){
                if (afnd->transiciones[i][j][k] == TRUE)
                    fprintf(pf, "%s ", estado_getNombre(afnd->estados[k]));
            }   
            fprintf(pf, "}\n");
        }
    }
    fprintf(pf, "\t}\n");
    fprintf(pf, "}\n");
}

AFND* AFNDInsertaLetra(AFND* afnd, char* letra){
    
    char* ret;
    
    if (afnd == NULL || letra == NULL)
        return NULL;
        
    if (buscarElemento(afnd->simbolos, letra) == -1){
        printf(">> El simbolo (%s) no esta reconocido en nuestro alfabeto!\n", letra);
        return afnd;
    }
    ret = insertarLetraPalabra(afnd->palabra, letra);
    
    if (ret == NULL){
        return NULL;
    }
    
    return afnd;
}

BOOL AFNDInicializaEstado(AFND* afnd){
    
    int i;
    ESTADO estado = NORMAL;
    
    if (afnd == NULL)
        return FALSE;
        
    afnd->countEstadosActuales = 0;
    
    for (i = 0; i < afnd->maxEstados; i++){
        estado = estado_getTipo(afnd->estados[i]);
        if (estado == INICIAL){
            afnd->actuales[i] = TRUE;
            afnd->countEstadosActuales++;
        }
        else{
            afnd->actuales[i] = FALSE;
        }
    }
    
    return TRUE;
}

void AFNDImprimeCadenaActual(FILE* pf, AFND* afnd){
    int i, simbolosActuales;
    char** simbolosPalabra = NULL;
    
    simbolosActuales = getCountSimbolosPalabraAct(afnd->palabra) - getIndiceActual(afnd->palabra);
    fprintf(pf, "[");
    fprintf(pf, "(%d) ",simbolosActuales);
    simbolosPalabra = getSimbolosPalabraAct(afnd->palabra);
    for (i = getIndiceActual(afnd->palabra); i < getCountSimbolosPalabraAct(afnd->palabra); i++)
        fprintf(pf, "%s ", simbolosPalabra[i]);
    fprintf(pf, "]\n");
}

void AFNDProcesaEntrada(FILE* pf, AFND* afnd){

    int i, j, k;
    char* simboloActual = NULL;
    int indiceElemento;
    BOOL* nuevosActuales = NULL;
    int flag;
    
    if (pf == NULL || afnd == NULL || afnd->actuales == NULL)
        return;
    
    
    for (i = getIndiceActual(afnd->palabra); i < getCountSimbolosPalabraAct(afnd->palabra); i++){
        
        
        fprintf(pf, "ACTUALMENTE EN { ");                    /* Primera linea */
        
        for (j = 0; j < afnd->maxEstados; j++){        /* Imprimimos los nombres de los estados diferenciando si son FINAL o NO */
            if(afnd->actuales[j] == TRUE){
                imprimeEstado(pf, afnd->estados[j]);
                fprintf(pf, " ");
            }
        }
        
        fprintf(pf, "}\n");
        
        AFNDImprimeCadenaActual(pf, afnd);                      /* Imprimimos la cadena */
        
        simboloActual = getSimboloActual(afnd->palabra);        /* Conseguimos el simbolo siguiente a ser procesar */
        
        nuevosActuales = (BOOL*) malloc(sizeof(BOOL) * afnd->maxEstados);
        for (k = 0; k < afnd->maxEstados; k++)
            nuevosActuales[k] = FALSE;
        
        for (j = 0; j < afnd->maxEstados; j++){       /* Vamos a transitar todos los estados actuales */ /* countEstadosActuales es nuevo */
            if(afnd->actuales[j] == TRUE){
                
                indiceElemento = buscarElemento(afnd->simbolos, simboloActual);
                flag = 0;
            
             /* Hacemos una OR logica con lo que tenemos y la siguiente transicion */
             
                for (k = 0; k < afnd->maxEstados; k++){
                    nuevosActuales[k] = nuevosActuales[k] || afnd->transiciones[j][indiceElemento][k];
                    if(nuevosActuales[k] == TRUE)
                        flag++;
                }
            }
        }
        free(afnd->actuales);
        afnd->actuales = nuevosActuales; /* Igualamos el array antiguo al nuevo */
        
        if(flag != 0){
            procesarSimbolo(afnd->palabra);
        }
        else{
            printf(">> LA CADENA INSERTADA NO SE PUEDE PROCESAR! (NO HAY POSIBLE ESTADO SIGUIENTE)\n");
            setIndiceActual(afnd->palabra, getCountSimbolosPalabraAct(afnd->palabra));
            return;
        }
    }
    flag = 0;
    fprintf(pf, "ACTUALMENTE EN { ");                    /* Primera linea */
    for (j = 0; j < afnd->maxEstados; j++){        /* Imprimimos los nombres de los estados diferenciando si son FINAL o NO */
        if(afnd->actuales[j] == TRUE){
            imprimeEstado(pf, afnd->estados[j]);
            if (estado_getTipo(afnd->estados[j]) == FINAL)
                flag++;
            fprintf(pf, " ");
        }
    }
    fprintf(pf, "}\n");
    AFNDImprimeCadenaActual(pf, afnd);
    
    if(flag != 0)
        printf(">> LA CADENA INSERTADA SE HA PROCESADO CORRECTAMENTE!\n");
    else
        printf(">> LA CADENA INSERTADA NO SE PUEDE PROCESAR! (NO HEMOS LLEGADO A UN ESTADO FINAL)\n");
        
    return;
}

void AFNDElimina(AFND* afnd){
    int i, j;
    
    if(!afnd) 
        return;
    
    for (i = 0; i < afnd->maxEstados; i++){
        destruirEstado(afnd->estados[i]);
    }
    free(afnd->estados);
    
    free(afnd->nombre);
    liberarAlfabeto(afnd->simbolos);
    liberarPalabra(afnd->palabra);
    free(afnd->actuales);
        
    for (i = 0; i < afnd->maxEstados; i++){
        for(j = 0; j < afnd->maxSimbolos; j++)
            free(afnd->transiciones[i][j]);
        free(afnd->transiciones[i]);
    }
    free(afnd->transiciones);

    free(afnd);
}

int get_indiceEstado(AFND* afnd, char* estado){
    int i;    

    if(!estado || !afnd)
        return -1;
    
    for(i = 0; i < afnd->maxEstados; i++){
        if(strcmp(estado, estado_getNombre(afnd->estados[i])) == 0)
            return i;
    }
    return -1;
}

int getCountSimbolosAFND(AFND* afnd){
    
    if(!afnd)
        return -1;
        
    return alfabeto_size(afnd->simbolos);
}
