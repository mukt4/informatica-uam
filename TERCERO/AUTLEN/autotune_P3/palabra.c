#include "palabra.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct _Palabra{
    int countSimbolosActuales;
    char** simbolosActuales;
    int posicionActual;
};

Palabra* crearPalabra(){
    Palabra* newPalabra = (Palabra*) malloc(sizeof(Palabra));
    
    newPalabra->countSimbolosActuales = 0;
    newPalabra->posicionActual = 0;
    newPalabra->simbolosActuales = NULL;
    
    return newPalabra;
}

void liberarPalabra(Palabra* palabra){
    int i;
    
    if(palabra == NULL)
        return;
    for(i = 0; i < palabra->countSimbolosActuales; i++)
        free(palabra->simbolosActuales[i]);
    free(palabra->simbolosActuales);
    free(palabra);
}

char* insertarLetraPalabra(Palabra* palabra, char* simbolo){
    if(palabra == NULL || simbolo == NULL)
        return NULL;
        
    palabra->countSimbolosActuales = palabra->countSimbolosActuales + 1;
    palabra->simbolosActuales = (char**)realloc(palabra->simbolosActuales, palabra->countSimbolosActuales * sizeof(char*));
    
    palabra->simbolosActuales[palabra->countSimbolosActuales - 1] = (char*)malloc(strlen(simbolo)  * sizeof(char) + 1);
    strcpy(palabra->simbolosActuales[palabra->countSimbolosActuales - 1], simbolo);
    
    return palabra->simbolosActuales[palabra->countSimbolosActuales - 1];
}

int procesarSimbolo(Palabra* palabra){
    if(palabra == NULL)
        return 0;
        
    if(palabra->posicionActual >= palabra->countSimbolosActuales)
        return 0;
    
    palabra->posicionActual = palabra->posicionActual + 1;
    
    return 1;
}

char** getSimbolosPalabraAct(Palabra* palabra){
    
    if(palabra == NULL)
        return NULL;
        
    return palabra->simbolosActuales;
}

int getCountSimbolosPalabraAct(Palabra* palabra){
    
    if(palabra == NULL)
        return 0;
        
    return palabra->countSimbolosActuales;
}

char* getSimboloActual(Palabra* palabra){
    if(!palabra || palabra->countSimbolosActuales == 0)
        return NULL;
    
    return palabra->simbolosActuales[palabra->posicionActual];
}

int getIndiceActual(Palabra* palabra){
    if(!palabra)
        return -1;
    
    return palabra->posicionActual;
}

int setIndiceActual(Palabra* palabra, int newIndice){
    if(!palabra || newIndice < 0)
        return -1;
    
    palabra->posicionActual = newIndice;
    
    return palabra->posicionActual;
}
