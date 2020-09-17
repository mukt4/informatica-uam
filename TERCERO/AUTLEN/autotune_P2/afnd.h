#ifndef AFND_H
#define AFND_H

#include "estado.h"

#include <stdio.h>

typedef struct _AFND AFND;

typedef enum{
    FALSE,
    TRUE
}BOOL;

/*
typedef enum{
    INICIAL,
    NORMAL,
    FINAL
}ESTADO;
*/

AFND* AFNDNuevo(char* nombre, int numEstados, int numSimbolos);
BOOL AFNDInsertaSimbolo(AFND* afnd, char* simbolo);
BOOL AFNDInsertaEstado(AFND* afnd, char* nombre, ESTADO estado);
BOOL AFNDInsertaTransicion(AFND* afnd, char* estado1, char* simbolo, char* estado2);
BOOL AFNDInsertaLTransicion(AFND* afnd, char* estado1, char* estado2);
void AFNDImprime(FILE* pf, AFND* afnd);
AFND* AFNDInsertaLetra(AFND* afnd, char* letra);
AFND* AFNDInicializaEstado(AFND* afnd);
void AFNDImprimeCadenaActual(FILE* pf, AFND* afnd);
void AFNDProcesaEntrada(FILE* pf, AFND* afnd);
void AFNDElimina(AFND* afnd);
int get_indiceEstado(AFND* afnd, char* estado);
int getCountSimbolosAFND(AFND* afnd);
AFND* AFNDInicializaCadenaActual(AFND* p_afnd_l);
void AFNDCierraLTransicion(AFND* p_afnd_l);

#endif