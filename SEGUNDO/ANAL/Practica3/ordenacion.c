/**
 *
 * Descripcion: Implementacion de funciones de ordenacion 
 *
 * Fichero: ordenacion.c
 * Autor: Carlos Aguirre
 * Version: 1.0
 * Fecha: 16-09-2017
 *
 */


#include "ordenacion.h"
#include "permutaciones.h"
#include <stdio.h>
#include <stdlib.h>

/***************************************************/
/* Funcion: BubbleSort   Fecha:30/9/2017           */
/* Vuestro comentario                              */
/***************************************************/
int BubbleSort(int* tabla, int ip, int iu)
{    
    int flag = 1;
    int i;
    int OB = 0;
    
    for (i = iu; flag == 1 && i >= ip+1; ){
        
        int j;
        
        flag = 0;
        
        for (j = ip; j < i; j++){
            OB++;
            if (tabla[j] > tabla[j+1]){
                swap(&tabla[j], &tabla[j+1]);
                flag = 1;
            }
        }
        i--;
    }

    return OB;
}

int MergeSort(int* tabla, int ip, int iu)
{
    int iMedio;
    int ob=0;
    
    if( ip > iu)
        return ERR;
        
    if( ip == iu)
        return OK;
        
    else{
        iMedio = ip + (iu - ip) / 2;
        ob += MergeSort (tabla, ip, iMedio);
        ob += MergeSort (tabla, iMedio + 1, iu);
        return ob + merge(tabla, ip,iu, iMedio);
    }
}
  
int merge(int* tabla, int ip, int iu, int imedio)
{
    int i, j, k, ob = 0;
    int *tablaAux;
    
    if( tabla == NULL)
        return ERR;
    
    tablaAux = (int*) malloc ( (iu - ip +1) * sizeof (int));
    
    if ( tablaAux == NULL){
        return ERR;
    }
    
    i = ip;
    j = imedio + 1;
    k = 0;
    
    while( i <= imedio && j <= iu){
        if( tabla[i] < tabla[j]){
            tablaAux[k] = tabla[i];
            i++;
        }else{
            tablaAux[k] = tabla[j];
            j++;
        }
        k++;
        ob++;
    }
    if (i > imedio){
        while ( j <= iu){
            tablaAux[k] = tabla[j];
            j++;
            k++;
        }
    }
    else if(j > iu){
        while( i <= imedio ){
            tablaAux[k] = tabla[i];
            i++;
            k++;
        }
    }
    
    copiar(tablaAux,tabla,ip,iu);
    free(tablaAux);
    
    return ob;
}
  
int copiar(int* tablaOrigen, int* tablaDestino, int ip, int iu)
{
    int i;
    int j = 0;
    
    if( tablaOrigen == NULL)
        return ERR;
    if( tablaDestino == NULL) 
        return ERR;
    
    for( i = ip ; i <= iu ; i++){
        tablaDestino[i] = tablaOrigen[j];
        j++;
    }
    
    return OK;
}

int QuickSort(int* tabla, int ip, int iu){
    int pos, err, OB=0;
    
    if(ip > iu)
        return ERR;
    
    if(ip == iu)
        return OK;
        
    else{
        err = partir(tabla, ip, iu, &pos);
        
        if(err != ERR)
            OB += err;
        else
            return ERR;
        
        OB++;
        
        if(ip < (pos - 1)){
            err = QuickSort(tabla, ip, pos - 1);
            
            if(err != ERR)
                OB += err;
            else
                return ERR;
        }
        
        OB++;
        
        if((pos + 1) < iu){
            err = QuickSort(tabla, (pos + 1), iu);
            
            if(err != ERR)
                OB += err;
            else
                return ERR;
        }
    }
    
    return OB;
}

int partir(int* tabla, int ip, int iu, int *pos)
{
    int k, i, err, OB = 0;
    
    err = medio_stat(tabla, ip, iu, pos);        /*Metodos medio, medio_avg y medio_stat*/
    
    if(err != ERR)
        OB += err;
    else
        return ERR;
    
    k = tabla[*pos];
    swap(&tabla[ip], &tabla[*pos]);
    
    (*pos) = ip;
    
    for (i = ip + 1; i <= iu; i++){
        OB++;
        if (tabla[i] < k){
            (*pos)++;
            swap(&tabla[i], &tabla[*pos]);
        }
    }
    
    swap(&tabla[ip], &tabla[*pos]);
    
    return OB;
}

int medio(int *tabla, int ip, int iu,int *pos)
{
    if(tabla == NULL)
        return ERR;
        
    *pos = ip;
    
    return 0;
}

int medio_avg(int* tabla, int ip, int iu, int* pos)
{
    if(tabla == NULL)                       /*Hacer todos los checkeos de NULL con assert*/
        return ERR;
    
    *pos = (ip + iu)/2;
    
    return 0;
}

int medio_stat (int *tabla, int ip, int iu, int *pos)
{
    int OB=0;
    
    if(tabla == NULL)
        return ERR;
    
    medio(tabla, ip, iu, pos);
    
    OB++;
    
    if(tabla[ip] < tabla[*pos]){
        
        OB++;
        
        if(tabla[*pos] < tabla[iu]){
            return OB;
        }
        else{
            *pos = iu;
            return OB;
        }
    }
    
    else {
        
        OB++;
        
        if(tabla[ip] < tabla[iu]){
            *pos = ip;
            return OB;
        }
        else{
            *pos = iu;
            return OB;
        }
    }
}

/* para la memoria tiene quehaber 5 graficas en cada una tiene que haber 
(la mas importante tiempo medio de relog, medio stat, medio y medio avg, y hay que añadir un ajuste
con una x²) 5 graphs en una

obs medias comparativa es una grafica que contiene otras 4 (merge min max medio y una cuadratica)
quiqsort con medio, con medio_avg y otra que ni guarra)

Grafica comparando los tiempos mejor,peor y medio en OBs para MergeSort
Grafica con el tiempo medio de reloj para MergeSort
Grafica comparando los tiempos mejor,pero y medio en OBs para QuickSort
Grafica con el tiempo medio de reloj para QuickSort
Grafica con el tiempo medio de reloj comparando los tres pivotes empleados
*/

