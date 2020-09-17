/**
 *
 * Descripcion: Implementacion de funciones de generacion de permutaciones
 *
 * Fichero: permutaciones.c
 * Autor: Carlos Aguirre
 * Version: 1.0
 * Fecha: 16-09-2017
 *
 */


#include "permutaciones.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/***************************************************/
/* Funcion: aleat_num Fecha:                       */
/* Autores: Tomás Higuera y Guillermo Hoyo         */
/*                                                 */
/* Rutina que genera un numero aleatorio           */
/* entre dos numeros dados                         */
/*                                                 */
/* Entrada:                                        */
/* int inf: limite inferior                        */
/* int sup: limite superior                        */
/* Salida:                                         */
/* int: numero aleatorio                           */
/***************************************************/
int aleat_num(int inf, int sup)
{
  return inf + rand() % (sup-inf+1);
}

/***************************************************/
/* Funcion: genera_perm Fecha:                     */
/* Autores: Tomás Higuera y Guillermo Hoyo         */
/*                                                 */
/* Rutina que genera una permutacion               */
/* aleatoria                                       */
/*                                                 */
/* Entrada:                                        */
/* int n: Numero de elementos de la                */
/* permutacion                                     */
/* Salida:                                         */
/* int *: puntero a un array de enteros            */
/* que contiene a la permutacion                   */
/* o NULL en caso de error                         */
/***************************************************/
int* genera_perm(int N)
{
  int i;
  int j;
  int* perm=(int*)malloc(N*sizeof(int));
  
  if(!perm)
    return NULL;
    
  for (i = 0 ; i < N ; i++){
    perm[i]=i+1;
  }
  
  for (i = 0 ; i < N ; i++){
    j=aleat_num(0,N-1);
    swap(&perm[i], &perm[j]);
  }
  
  return perm;
}

/***************************************************/
/* Funcion: genera_permutaciones Fecha:            */
/* Autores: Tomás Higuera y Guillermo Hoyo         */
/*                                                 */
/* Funcion que genera n_perms permutaciones        */
/* aleatorias de tamanio elementos                 */
/*                                                 */
/* Entrada:                                        */
/* int n_perms: Numero de permutaciones            */
/* int N: Numero de elementos de cada              */
/* permutacion                                     */
/* Salida:                                         */
/* int**: Array de punteros a enteros              */
/* que apuntan a cada una de las                   */
/* permutaciones                                   */
/* NULL en caso de error                           */
/***************************************************/
int** genera_permutaciones(int n_perms, int N)
{
  int i;
  int **perm=NULL;
  perm=(int**)malloc(n_perms*sizeof(int*));
  if(!perm)
    return NULL;
  for(i=0;i<n_perms;i++){
    perm[i]=genera_perm(N);
  }
  return perm;
}

/***************************************************/
/* Funcion: swap Fecha: 18/10/17                   */
/* Autores: Tomás Higuera y Guillermo Hoyo         */
/*                                                 */
/* Funcion que realiza un swap entre dos           */
/* elementos de un array                           */
/*                                                 */
/* Entrada:                                        */
/* int* elem1: Elemento del array para swap        */
/* int* elem2: Elemento2 del array para swap       */
/* Salida:                                         */
/* int: Devuelve ERR si hay algun error y OK       */
/* si no lo hay                                    */
/***************************************************/
int swap(int* elem1, int* elem2){
  int k;
  if(!elem1 || !elem2){
    return ERR;
  }
  k=*elem1;
  *elem1=*elem2;
  *elem2=k;
  return OK;
}