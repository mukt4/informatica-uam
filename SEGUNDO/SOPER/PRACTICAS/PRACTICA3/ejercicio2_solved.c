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

#define KEY  1245/*Key*/

/*Function to detect the signal SIGUSR1*/
void manejador_SIGUSR1(int sig);
/*Function to make a proces the priority one for a random time*/
int tiempo_aleatorio();

struct info{
 char nombre[80];
 int id;
};

union semun{
        int val;
        struct semid_ds *semstat;
        unsigned short *array;
}arg;

struct info *info; /*Global variable*/

int main(int argc, char *argv[]){
    int param = -1;/*Entroce parameter of the function ejercicio2*/
    int i = -1; /*Variable for loops*/
    int pid = -1; /*Variable that saves the pid of the process*/
    int key = -1; /*Key from the Shared Memory zone and semaphore*/
    int semid; /*Id of the created semaphore*/
    int memory_id = 0; /*Variable that contains the id of the shared memory zone*/
    unsigned short *array;/*Struct to inicializate the semaphores*/
    sigset_t sigset; /*Sigset_t used to wait the specifiend signal in sigwait()*/
    int sig; /*Variable to wait the SIGUSER1 signall*/
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGUSR1);
    
    array = malloc(1 * sizeof(unsigned short));
    array[0] = 1;
    
    /*Cheking that the parameters for the execution are right*/
    if(argc <= 1 || argc > 2){
        perror("\nError en el paso de parametros\n");
        free(array);
        exit(EXIT_FAILURE);
    }
    
    param = atoi(argv[1]);
    if(param == -1){
        free(array);
        perror("\nError al pasar el parametro a int\n");
        exit(EXIT_FAILURE);
    }
    
    /*Inicializating key to create the Shared Memory and the Semaphore*/
    key = ftok(FILEKEY, KEY);
            
    /*If the keys hasnt inicilizated correctly return*/
    if (key == -1) {
        free(array);
        perror ("\nError en key \n");
        exit(EXIT_FAILURE);
    }
        
    /*Creating the shared Memory Zone*/
    memory_id = shmget(key, sizeof(struct info), IPC_CREAT | SHM_W | SHM_R);
            
    /*If the id hasnt inicializated correctly return*/
    if (memory_id == -1) {
        free(array);
        perror ("\nError con memory_id\n");
        exit(EXIT_FAILURE);
    }
            
    /*Creating the semaphores for mutual exclusion*/
    if(Crear_Semaforo(key, 1, &semid) == ERROR){
        free(array);
        perror("\nError al crear semaforos \n");
        exit(EXIT_FAILURE);
    }
            
    /*Inicializating semaphore*/
    if(Inicializar_Semaforo(semid, array) == ERROR){
        free(array);
        perror("\nError al inicializar semaforos\n");
        exit(EXIT_FAILURE);
    }
            
    printf("\nLa id de la memoria compartida es: %d", memory_id);
    
    /*Creating n Sons Process*/
    for(i = 0; i < param; i++){
        pid = fork();
        
        /*Cheking the fork() is correctly done*/
        if(pid < 0){
            perror("\nError en el fork\n");
            free(array);
            exit(EXIT_FAILURE);
        }
        
        /*Father Process*/
        else if(pid != 0){
            /*Declaring the zone that is going to be shared with the sons processes*/
            info = shmat(memory_id, (char*)0, SHM_W | SHM_R);
            
            /*Testinf the varible info is now attached to a virtual address*/
            if(info == NULL){
                perror("\nError al unir la variable compartida con una dirreccion de memoria virtual\n");
                free(array);
                exit(EXIT_FAILURE);
            }
            
            /*When the Father recibes the signal he shows the complete struct info*/
            signal(SIGUSR1, manejador_SIGUSR1);
            sigwait(&sigset , &sig);
            if(sig == SIGUSR1){
                printf("Detectada la señal SIGUSR1");
                Down_Semaforo(semid, 0, SEM_UNDO);
                printf("\nSOY EL PADRE, HE RECIBIDO LA SEÑAL Y ME DISPONGO A MOSTRAR POR PANTALLA LA ESTRUCTURA DE MEMORIA COMPARTIDA");
                printf("\n\tnombre: %s \tid: %d\n", info->nombre, info->id);
                Up_Semaforo(semid, 0, SEM_UNDO);
            }
        }
        
        /*Son Process*/
        else{
            int tiempo = -1;
            char nombre_c[80] = "";
            
            /*This process sleps a random time*/
            tiempo = tiempo_aleatorio();
            sleep(tiempo);
            
            /*We make this process the only one with acces to the memory zone, the rest will get bloqued for a random time*/
            Down_Semaforo(semid, 0, SEM_UNDO);
            printf("\nIngrese nombre de usuario: ");
            fflush(stdin);
            scanf("%s", nombre_c);
            
            /*Atatch to the memory zone*/
            info = shmat(memory_id, (char*)0, 0);
            
            /*Actualizing the global variable info*/
            strcpy(info->nombre, nombre_c);
            if(strcmp(info->nombre,nombre_c) != 0){
                free(array);
                perror("\nError al incializar el valor de nombre de la variable global\n");
                exit(EXIT_FAILURE);
            }
            info->id++;
            if(info->id == -1){
                free(array);
                perror("\nError al modificar el valor de la id de la variable global\n");
                exit(EXIT_FAILURE);
            }
            
            /*Unatatch of the son process to the shared memory zone*/
            if(shmdt((char*)info) == -1){
                free(array);
                perror("\nError en el desenganchado del proceso hijo a la memoria compartida\n");
                exit(EXIT_FAILURE);
            }
            /*Up so that the rest of the proccesses can execute*/
            Up_Semaforo(semid, 0, SEM_UNDO);
            /*Throwing the signal to the Father Process so that he can show the complete info struct*/
            kill(getppid(), SIGUSR1);

            /*End of the son process*/
            free(array);
            exit(EXIT_SUCCESS);
        }
        
    }
    
    /*Waiting all the son processes*/
    while(wait(NULL)>0);
            
    /*Destroying the semaphore*/
    Borrar_Semaforo(semid);
    free(array);
    /*Destroying the shared memory zone*/
    if(shmdt((char*)info) == -1){
        perror("\nError en el desenganchado del proceso padre a la memoria compartida\n");
        exit(EXIT_FAILURE);
    }
    shmctl(memory_id, IPC_RMID, (struct shmid_ds *)NULL);
    return 0;

}

void manejador_SIGUSR1(int sig){
    printf("Detectada la señal SIGUSR1");
}

int tiempo_aleatorio(){
    return (rand() % 30);
}