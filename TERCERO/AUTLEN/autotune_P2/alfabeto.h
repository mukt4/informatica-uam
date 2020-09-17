#ifndef ALFABETO_H
#define ALFABETO_H

#include <stdio.h>

typedef struct _Alfabeto Alfabeto;

typedef enum{
    DESORDENADO,
    ORDENADO
}ORDEN;

Alfabeto* crearAlfabeto(int tamanio);
void liberarAlfabeto(Alfabeto* alfabeto);
void ordenarAlfabeto(Alfabeto* alfabeto);
char* insertarElemento(Alfabeto* alfabeto, char* insertar);
int alfabeto_size(Alfabeto* alfabeto);
void imprimirAlfabeto(Alfabeto* alfabeto);
ORDEN isOrdendado(Alfabeto* alfabeto);
char* alfabeto_get_i(Alfabeto* alfabeto, int i);
int buscarElemento(Alfabeto* alfabeto, char* elemento);
void imprimeAlfabeto(FILE* pf, Alfabeto* alfabeto);

#endif