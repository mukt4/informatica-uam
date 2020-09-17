/**
 *
 * Descripcion: Implementacion funciones para busqueda 
 *
 * Fichero: busqueda.c
 * Autor: Carlos Aguirre
 * Version: 1.0
 * Fecha: 11-11-2016
 *
 */

#include "busqueda.h"
#include "permutaciones.h"

#include <stdlib.h>
#include <math.h>
#include <string.h>

/**
 *  Funciones de geracion de claves
 *
 *  Descripcion: Recibe el numero de claves que hay que generar
 *               en el parametro n_claves. Las claves generadas
 *               iran de 1 a max. Las claves se devuelven en 
 *               el parametro claves que se debe reservar externamente
 *               a la funcion.
 */
  
/**
 *  Funcion: generador_claves_uniforme
 *               Esta fucnion genera todas las claves de 1 a max de forma 
 *               secuencial. Si n_claves==max entonces se generan cada clave
 *               una unica vez.
 */
void generador_claves_uniforme(int *claves, int n_claves, int max)
{
	int i;
	
	
	for(i = 0; i < n_claves; i++) claves[i] = 1 + (i % max);

	return;
}

/**
 *  Funcion: generador_claves_potencial
 *               Esta funcion genera siguiendo una distribucion aproximadamente
 *               potencial. Siendo los valores mas pequenos mucho mas probables
 *               que los mas grandes. El valor 1 tiene una probabilidad del 50%,
 *               el dos del 17%, el tres el 9%, etc.
 */
void generador_claves_potencial(int *claves, int n_claves, int max)
{
  int i;

  for(i = 0; i < n_claves; i++) {
    claves[i] = (1+max) / (1 + max*((double)rand()/RAND_MAX));
  }

  return;
}

PDICC ini_diccionario (int tamanio, char orden)
{
    PDICC diccionario = (PDICC)malloc(sizeof(DICC));
    
    if(diccionario == NULL)
        return NULL;
    
    diccionario->tamanio = tamanio;
    diccionario->orden = orden;
    diccionario->n_datos = 0;
    diccionario->tabla = (int*)malloc(tamanio * sizeof(int));
    
    return diccionario;
}

void libera_diccionario(PDICC pdicc)
{
	if (pdicc == NULL)
	    return;
	
	if(pdicc->tabla != NULL)
	    free(pdicc->tabla);
	
	free(pdicc);
}

int inserta_diccionario(PDICC pdicc, int clave)
{
    int j = 0, a = 0, ob = 0;
    
	if (pdicc == NULL)
	    return ERR;
	    
	
	pdicc->tabla[pdicc->n_datos] = clave;
	pdicc->n_datos++;
	    
	
	
	if(pdicc->orden == ORDENADO){
	    
	    a = pdicc->tabla[(pdicc->n_datos) - 1];
	    j = (pdicc->n_datos) - 2;
	    
		while(j >= 0 && pdicc->tabla[j] > a){
	        pdicc->tabla[j+1] = pdicc->tabla[j];
			j--;
			ob++;
		}
		
	    pdicc->tabla[j+1] = a; 
	    return ob;
	}
	return ob;
}

int insercion_masiva_diccionario (PDICC pdicc,int *claves, int n_claves)
{
	int i = 0, ob = 0, retorno = 0;
	for (i = 0; i <= n_claves - 1; i++){
		retorno = inserta_diccionario(pdicc, claves[i]);
		if (retorno == ERR) return ERR;
		ob +=retorno;
	}
	
	return ob;
}

int busca_diccionario(PDICC pdicc, int clave, int *ppos, pfunc_busqueda metodo)
{
	return metodo(pdicc->tabla, 0, (pdicc->n_datos) - 1, clave, ppos);
}

/* Funciones de busqueda del TAD Diccionario */
int bbin(int *tabla,int P,int U,int clave,int *ppos)
{
	int m, u=U, p=P, OB = 0;

	if(ppos == NULL){
		*ppos = NO_ENCONTRADO;
		return ERR;
	}
	
	while(p <= u){
		m=(p + u)/2;
		OB++;
		if(tabla[m] == clave){
			*ppos = m;
			return OB;
		}
		else if(clave < tabla[m]){
			u = m - 1;
		}
		else{
			p = m + 1;
		}
	}
	*ppos = NO_ENCONTRADO;
	return OB;
}

int blin(int *tabla,int P,int U,int clave,int *ppos)
{
	int i = 0, OB = 0;
	
	if(tabla == NULL)
	    return ERR;
	   
	for(i = P; i <= U; i++){
		OB++;
		if(tabla[i] == clave){
			*ppos = i;
			return OB;
		}
	}
	
	*ppos = NO_ENCONTRADO;
	return OB;
}

int blin_auto(int *tabla,int P,int U,int clave,int *ppos)
{
	int OB = 0;
	
	OB = blin(tabla, P, U, clave, ppos);
	OB++;
	
	if(*ppos == 0){
		return OB;
	}
	else if(*ppos == NO_ENCONTRADO){
		return OB;
	}
	else{
		swap(&tabla[(*ppos) - 1], &tabla[(*ppos)]);
		return OB;
	}
}