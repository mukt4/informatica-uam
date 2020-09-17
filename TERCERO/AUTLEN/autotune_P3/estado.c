#include "estado.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


struct _Estado{
    char* nombre;
    ESTADO tipoEstado;
};

Estado* crearEstado(char* nombre, ESTADO tipo){
    Estado* estado;

    estado = (Estado*)malloc(sizeof(Estado));
    estado->nombre = (char*)malloc(strlen(nombre) * sizeof(char) + 1);
    strcpy(estado->nombre, nombre);
    estado->tipoEstado = tipo;
    
    return estado;
}

char* estado_getNombre(Estado* estado){
    if(!estado)
        return NULL;
        
    return estado->nombre;
}

ESTADO estado_getTipo(Estado* estado){
    if(!estado)
        return -1;
    
    return estado->tipoEstado;
}

void destruirEstado(Estado* estado){
    if(!estado) 
        return;

    free(estado->nombre);
    free(estado);
}

void imprimeEstado(FILE* pf, Estado* estado){
    if(!estado || !pf)
        return;
    
    switch(estado->tipoEstado){
        case FINAL:
            fprintf(pf, "%s*", estado->nombre);
            break;
        case INICIAL:
            fprintf(pf, "->%s", estado->nombre);
            break;
        case NORMAL:
            fprintf(pf, "%s", estado->nombre);
            break;
        default:
            break;
    }
}
