/**
 *
 * Descripcion: Implementacion de funciones de tiempo
 *
 * Fichero: tiempos.c
 * Autor: Carlos Aguirre Maeso
 * Version: 1.0
 * Fecha: 16-09-2017
 *
 */

#include "tiempos.h"
#include "ordenacion.h"
#include "permutaciones.h"
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

/***************************************************/
/* Funcion: tiempo_medio_ordenacion Fecha:         */
/*                                                 */
/* Vuestra documentacion (formato igual            */
/* que en el primer apartado):                     */
/***************************************************/

short tiempo_medio_ordenacion(pfunc_ordena metodo, int n_perms,int N, PTIEMPO ptiempo){
  int ** perms=genera_permutaciones(n_perms,N);
  clock_t t_ini, t_fin;
  int i,OB,min,max;
  double count=0;
  if(!perms)
    return ERR;
  min=INT_MAX;
  max=0;
  t_ini=clock();
  for(i=0;i<n_perms;i++){
    OB=metodo(perms[i],0,N-1);
    if(OB<min)
      min=OB;
    if(OB>max)
      max=OB;
    count+=OB;
  }
  t_fin=clock();
  ptiempo->N = N;
  ptiempo->n_elems = n_perms;
  ptiempo->tiempo = ((double)(t_fin - t_ini)/n_perms); /*NO FUNCIONA LO DE CLOCKS_PER_SEC, SALE 0,000000*/
  ptiempo->medio_ob = count/n_perms;
  ptiempo->min_ob = min;
  ptiempo->max_ob = max;
  
  for(i = 0; i< n_perms; i++){
    free(perms[i]);
  }
  free(perms);
  return OK;
}
/***************************************************/
/* Funcion: genera_tiempos_ordenacion Fecha:       */
/*                                                 */
/* Vuestra documentacion                           */
/***************************************************/

short genera_tiempos_ordenacion(pfunc_ordena metodo, char* fichero, int num_min, int num_max, int incr, int n_perms){
  int j,i=num_min,num=0;
  PTIEMPO tiempo=NULL;
  if(!fichero)
    return ERR;
  num=((num_max-num_min)/incr)+1;
  tiempo=(PTIEMPO)malloc(num*sizeof(TIEMPO));
  for(j=0;j<num;j++){
    tiempo_medio_ordenacion(metodo,n_perms,i,&tiempo[j]);
    i+=incr;
  }
  guarda_tabla_tiempos(fichero,tiempo,num);
  free(tiempo);
  return OK;
}
/***************************************************/
/* Funcion: guarda_tabla_tiempos Fecha:            */
/*                                                 */
/* Vuestra documentacion (formato igual            */
/* que en el primer apartado):                     */
/***************************************************/

short guarda_tabla_tiempos(char* fichero, PTIEMPO tiempo, int n_tiempos){
  int i;
  FILE *pf=NULL;
  if(!fichero)
    return ERR;
  pf=fopen(fichero,"w");
  if(!pf)
    return ERR;
  for(i=0;i<n_tiempos;i++){
    fprintf(pf,"%d %f %f %d %d\n",tiempo[i].N,tiempo[i].tiempo,tiempo[i].medio_ob,tiempo[i].max_ob,tiempo[i].min_ob);
  }
  fclose(pf);
  return OK;
}



