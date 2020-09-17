/**
 *
 * Descripcion: Funciones de cabecera para ordenacion 
 *
 * Fichero: ordenacion.h
 * Autor: Carlos Aguirre
 * Version: 1.0
 * Fecha: 16-09-2016
 *
 */

#ifndef ORDENACION_H
#define ORDENACION_H

/* constantes */

#ifndef ERR
  #define ERR -1
  #define OK (!(ERR))
#endif

/* definiciones de tipos */
typedef int (* pfunc_ordena)(int*, int, int);

/* Funciones */

int BubbleSort(int* tabla, int ip, int iu);

int MergeSort(int* tabla, int ip, int iu);

int merge(int* tabla, int ip, int iu, int imedio);

int copiar(int* tabla1, int* tabla2, int ip, int iu);

int QuickSort(int* tabla, int ip, int iu);

int partir(int* tabla, int ip, int iu, int *pos);

int medio(int *tabla, int ip, int iu, int *pos);

int medio_stat (int *tabla, int ip, int iu, int *pos);

int medio_avg(int *tabla, int ip, int iu, int *pos);


#endif
