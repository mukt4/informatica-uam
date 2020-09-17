#ifndef AFND_H
#define AFND_H

#include "estado.h"
#include "alfabeto.h"
#include "palabra.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct _AFND AFND;

typedef enum{
    FALSE,
    TRUE
}BOOL;

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

/*FUNCIONES AFND10*/
AFND* AFND1ODeSimbolo(char* simbolo);
AFND* AFND1ODeLambda();
AFND* AFND1ODeVacio();
AFND* AFNDAAFND1O(AFND* p_afnd);
AFND* AFND1OUne(AFND* p_afnd1O_1, AFND* p_afnd1O_2);
AFND* AFND1OConcatena(AFND* p_afnd_origen1, AFND* p_afnd_origen2);
AFND* AFND1OEstrella(AFND* p_afnd_origen);
void AFNDADot(AFND* p_afnd);

/* GETTERS Y SETTERS */

#endif