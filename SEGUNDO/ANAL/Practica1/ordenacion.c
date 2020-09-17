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


