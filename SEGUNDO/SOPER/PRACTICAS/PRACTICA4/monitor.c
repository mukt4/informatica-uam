#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <sys/sem.h>

#include "monitor.h"
#include "gestor.h"
#include "semaforos.h"

void imprimir_cotizaciones(int memid, int nCaballos){
    int i;
    Informacion* info;
    printf("Estado actual de las cotizaciones\n");
    printf(" _______________________________________________\n");
	printf("|Caballo\t|Dinero apostado |Cotizacion\t|\n");
	printf("|---------------|---------------|---------------|\n");
	info = shmat(memid, (char*)0, SHM_W | SHM_R);
	for(i = 0; i < nCaballos; i++){
	    printf("|%d\t\t|%lf \t|%lf\t|\n",i + 1, info->dineroApostado[i], info->cotizacionCaballo[i]);
	}
	printf(" _______________________________________________\n");
	printf("|Dinero total apostado: %lf\t\t|\n", info->totalDinero);
	printf(" _______________________________________________\n");
	shmdt(info);
}

void imprimir_carrera(int memid, int semid, int nCaballos){
    int i;
    Informacion* info;
    printf("Estado de la carrera\n");
    printf(" _______________________________________________\n");
	printf("|Caballo\t|Posicion\t|Cotizacion\t|\n");
	printf("|---------------|---------------|---------------|\n");
	info = shmat(memid, (char*)0, SHM_W | SHM_R);
	for(i = 0; i < nCaballos; i++){
	    printf("|%d\t\t|%d\t\t|%lf\t|\n",i + 1, info->posicionCaballos[i], info->cotizacionCaballo[i]);
	}
	printf(" _______________________________________________\n");
	shmdt(info);
	Up_Semaforo(semid, 4, SEM_UNDO);
}

void imprimir_final(int memid, int nCaballos, int nApostantes, double dinero){
    Informacion* info;
    int i, j = 0, k, max = 0, idCaballos[MAX_CABALLOS];
    
    printf("\nHa acabado la carrera!!\n");
    info = shmat(memid, (char*)0, SHM_W | SHM_R);
	for(i = 0; i < nCaballos; i++){
	    if(info->posicionCaballos[i] > max)
	        max = info->posicionCaballos[i];
	}
	for(i = 0; i < nCaballos; i++){
	    if(info->posicionCaballos[i] == max){
	        idCaballos[j] = i;
	        j++;
	    }
	}
	if(j == 1){
	    printf("\nCaballo ganador: %d", idCaballos[0] + 1);
	}
	else{
	    printf("\nCaballos ganadores: ");
	    for(i = 0; i < j; i++){
	        printf("%d ",i+1);
	    }
	}
	printf("\nEstado final de las apuestas:");
	printf("\nTotal de dinero apostado: %lf\n", info->totalDinero);
	printf("\nApuestas ganadoras: \n");
	
	for(i = 0, j = 0; i < nApostantes; i++){
	    while(info->apuestas[i][j].id != 0){
	        for(k = 0; k < nApostantes; k++){
	            if(info->apuestas[i][j].n_caballo == idCaballos[k]){
	                printf("\nApuesta ganadora:\n\tId apuesta: %ld\n\tCaballo %d\n\tApuesta: %lf\n\tNombre apostador: %s\n\tId apostador: %d\n", info->apuestas[i][j].id, info->apuestas[i][j].n_caballo + 1, info->apuestas[i][j].apuesta, info->apuestas[i][j].nombre, info->apuestas[i][j].idApostante + 1);
	                info->ganancia[i] += (info->cotizacionCaballo[idCaballos[k]] * info->apuestas[i][j].apuesta);
	                info->totalDineroApostador[i] += (info->cotizacionCaballo[idCaballos[k]] * info->apuestas[i][j].apuesta);
	                info->totalDinero -= (info->cotizacionCaballo[idCaballos[k]] * info->apuestas[i][j].apuesta);
	            }
	        }
	        j++;
	    }
	}
	printf("\nResultados de los apostadores\n");
	for(i = 0; i < nApostantes; i++){
	    printf("\nDinero final apostador %d:  %lf\n",i+1,  info->totalDineroApostador[i]);
	}
	printf("Total de dinero recaudado tras las apuestas: %lf\n", info->totalDinero);
	printf("\nFIN DEL PROGRAMA\n");
	
	shmdt(info);
}