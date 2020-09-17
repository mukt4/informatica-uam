#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <sys/shm.h>
#include <stdlib.h>
#include "semaforos.h"

#define FILEKEY "/bin/cat" /*Util para ftok */
#define KEY 1100
#define N_SEMAFOROS 2

int main(){
    
    /*Declaración de variables */
    int sem_id; /* ID de la lista de semáforos */
    unsigned short valor[2] = {1,1};
    int key = ftok(FILEKEY, KEY);
    
    /* Creamos una lista o conjunto con dos semáforos */
    if(Crear_Semaforo(key, N_SEMAFOROS, &sem_id) ==  ERROR)
        printf("EING");
    
    /* Inicializamos los semáforos */
    if(Inicializar_Semaforo(sem_id, valor) == ERROR)
        printf("EING");
    
    semctl (sem_id, N_SEMAFOROS, GETALL, valor);
    printf ("Los valores de los semáforos son %d y %d\n", valor[0], valor[1]);
    
    /* Operamos sobre los semáforos */
    Down_Semaforo(sem_id, 0, SEM_UNDO);
    semctl (sem_id, N_SEMAFOROS, GETALL, valor);
    printf ("Los valores de los semáforos son %d y %d\n", valor[0], valor[1]);
    Up_Semaforo(sem_id, 1, SEM_UNDO);
    
    /* Veamos los valores de los semáforos */
    semctl (sem_id, N_SEMAFOROS, GETALL, valor);
    printf ("Los valores de los semáforos son %d y %d\n", valor[0], valor[1]);
    
    /* Eliminar la lista de semáforos */
    Borrar_Semaforo(sem_id);
    
    return 0;
}
/* fin de la función main */