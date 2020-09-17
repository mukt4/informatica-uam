#include <sys/wait.h>
#include <time.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <errno.h>
#include <sys/shm.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <limits.h>
#include <pthread.h>

#include "gestor.h"
#include "seniales.h"
#include "semaforos.h"

void* gestion(void* gestion){
    int id_cola;
    int id_memoria;
    int id_semaforo;
    Mensaje mensaje;
    Informacion* info;
    
    if(gestion == NULL){
        pthread_exit(NULL);
    }  
    
    signal(SIGTRAP, manejador_SIGTRAP);
    
    id_cola = ((Gestion*)gestion)->id_cola;
    id_memoria = ((Gestion*)gestion)->id_memoria;
    id_semaforo = ((Gestion*)gestion)->id_semaforo;

    while(1){
        /*BAJAMOS EL SEMAFORO DE CONTROL DE INTERBLOQUEO*/
        Down_Semaforo(id_semaforo, 3, SEM_UNDO);
        /*BAJAMOS EL MUTEX PARA RECIBIR UN MENSAJE*/
        Down_Semaforo(id_semaforo, 2, SEM_UNDO);
        
        msgrcv(id_cola, (struct Mensaje*)&mensaje,  sizeof(Mensaje) - sizeof(long), 0, 0);
        
        Up_Semaforo(id_semaforo, 2, SEM_UNDO);
        
        /*BAJAMOS EL MUTEX DE MEMORIA COMPARTIDA*/
        Down_Semaforo(id_semaforo, 1, SEM_UNDO);
        
        /*ENLAZAMOS LA INFORMACION DE LA MEMORIA COMPARTIDA*/
        info = shmat(id_memoria, (char*)0, SHM_W | SHM_R);
        if(info == NULL){
            perror("Error al inicializar la zona de memoria compartida");
            exit(EXIT_FAILURE);
        }
        
        info->totalDinero += mensaje.apuesta;
        info->dineroApostado[mensaje.n_caballo] += mensaje.apuesta;
        info->apuestas[mensaje.idApostante][mensaje.nApuesta] = mensaje;
        info->cotizacionCaballo[mensaje.n_caballo] = info->totalDinero / info->dineroApostado[mensaje.n_caballo];
        info->totalDineroApostador[mensaje.idApostante] -= mensaje.apuesta;

        shmdt(info);
        
        Up_Semaforo(id_semaforo, 1, SEM_UNDO);
    }
    pthread_exit(NULL);
}
