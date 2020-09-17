#ifndef ESTADO_H
#define ESTADO_H

#include <stdio.h>

typedef struct _Estado Estado;

typedef enum {
    INICIAL,
    NORMAL,
    FINAL,
    INICIAL_Y_FINAL
}ESTADO;

Estado* crearEstado(char* nombre, ESTADO tipo);

char* estado_getNombre(Estado* estado);

ESTADO estado_getTipo(Estado* estado);

void destruirEstado(Estado* estado);

void imprimeEstado(FILE* pf, Estado* estado);

#endif