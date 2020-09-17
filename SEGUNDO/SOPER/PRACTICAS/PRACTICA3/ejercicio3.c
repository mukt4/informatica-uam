#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <string.h>
#include <errno.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <time.h>
#include <sys/wait.h>
#include <signal.h>

#include "semaforos.h"

#define FILEKEY "/bin/cat"

#define KEY 12/*Key*/
#define N 36

/*Global variables*/
char *array;

int main(int argc, char *argv[]){
    int i; /*Variable for loops*/
    int pid = -1; /*Variable that saves the pid of the process*/
    int semid; /*Id of the semaphores*/
    int key = ftok(FILEKEY, KEY); /*Key for Semaphore and shared Memory Zone*/
    int memory_id = -1; /*Variable that contains the id of the shared memory zone*/
    char productor[N] = {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9'};
    union semun{
        int val;
        struct semid_ds *semstat;
        unsigned short *array;
    }arg; /*Struct to inicializate the semaphores*/
    
    if (key == -1) {
        perror ("\nError en key \n");
    }
    
    /*Inicialization of the values of the semaphores*/
    arg.array = malloc(3 * sizeof(unsigned short));
    arg.array[0] = 1; /*Mutual Exclution*/
    arg.array[1] = N; /*Number of free spaces*/
    arg.array[2] = 0; /*Number of occupied spaces*/
    
    /*Creating the semaphores*/
    if(Crear_Semaforo(key, 3, &semid) == ERROR){
        free(arg.array);
        perror("\nError al crear semaforos \n");
        exit(EXIT_FAILURE);
    }
    
    /*Inicializating semaphores*/
    if(Inicializar_Semaforo(semid, arg.array) == ERROR){
        free(arg.array);
        perror("\nError al inicializar semaforos\n");
        exit(EXIT_FAILURE);
    }
    
    pid = fork(); 
    if(pid < 0){
        free(arg.array);
        perror("Error al realizar el fork");
        exit(EXIT_FAILURE);
    }
    
    /*Creating the shared Memory Zone*/
    memory_id = shmget (key, (sizeof(char*)*2), IPC_CREAT | SHM_W | SHM_R);
            
    /*If the id hasnt inicializated correctly return*/
    if (memory_id == -1) {
        free(arg.array);
        perror ("\nError con memory_id \n");
        exit(EXIT_FAILURE);
    }
    
    /*Father process -- producer*/
    if(pid != 0){
        for(i = 0; i < N; i++){
            /*Checking if there are free spaces and apliying mutual exclusion*/
            Down_Semaforo(semid, 1, SEM_UNDO);
            Down_Semaforo(semid, 0, SEM_UNDO);
            
            /*Declaring the zone that is going to be shared with the sons processes*/
            array = shmat(memory_id, (char*)0, SHM_W | SHM_R);
            
            /*Producing the element*/
            array[i] = productor[i];
            printf("\nValor producido: %c", array[i]);
            
            /*Increasing the value of the sepaphore of occupied spaces and the one for the mutual exclusion*/
            Up_Semaforo(semid, 0, SEM_UNDO);
            Up_Semaforo(semid, 2, SEM_UNDO);
            
        }
    }
    
    /*Son process -- consumer*/
    else{
        for(i = 0; i < N; i++){
            char consumido = '\0';
            
            /*Checking if there are occupied spaces and apliying mutual exclusion*/
            Down_Semaforo(semid, 2, SEM_UNDO);
            Down_Semaforo(semid, 0, SEM_UNDO);
            
            /*Declaring the zone that is going to be shared with the sons processes*/
            array = shmat(memory_id, (char*)0, SHM_W | SHM_R);
            
            /*Consuming the element*/
            consumido = array[i];
            array[i] = '\0';
            printf("\nValor consumido: %c", consumido);
            
            /*Increasing the value of the sepaphore of occupied spaces and the one for the mutual exclusion*/
            Up_Semaforo(semid, 0, SEM_UNDO);
            Up_Semaforo(semid, 1, SEM_UNDO);
        }
        /*Destroying the shared memory zone*/
        if(shmdt((char*)array) == -1){
            free(arg.array);
            perror("\nError en el desenganchado del proceso hijo a la memoria compartida del array\n");
            exit(EXIT_FAILURE);
        }
        
        free(arg.array);
        exit(EXIT_SUCCESS);
    }
    
    /*Waiting all the son processes*/
    while(wait(NULL)>0);
            
    /*Destroying the semaphore*/
    Borrar_Semaforo(semid);
    free(arg.array);
    
    /*Destroying the shared memory zone*/
    if(shmdt((char*)array) == -1){
        perror("\nError en el desenganchado del proceso padre a la memoria compartida del array\n");
    }
    shmctl(memory_id, IPC_RMID, (struct shmid_ds *)NULL);
    return 0;
    
}