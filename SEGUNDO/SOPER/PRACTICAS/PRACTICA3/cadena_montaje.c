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
#include <sys/msg.h>

#include "semaforos.h"

#define FILEKEY "/bin/cat"

#define KEY  4732

#define N 32
#define TAM_MAX 2000

typedef struct _Mensaje{
    long id; /*Campo obligatorio a long que identifica el tipo de mensaje*/
    char text[TAM_MAX];/*Informacion a transmitir en el mensaje*/
}mensaje;

int main(int argc, char *argv[]){
    int j = 0; /*Variable for loops*/
    int id_cola = -1; /*Variable to safe the id of the message queue*/
    int pid = -1; /*Variable to safe the returned value form the forks*/
    mensaje mensaje; /*Struct mensaje*/
    int key = -1; /*Key for the semaphore*/
    key_t clave; /*Key for the queue of the message*/
    FILE *fp; /*Files*/
    int semid = -1; /*Valiable to safe the id of the semaphore*/
    unsigned short *array;/*Struct to inicializate the semaphores*/

    /*Inicializing the values of the semafore*/
    array = malloc(1 * sizeof(unsigned short));
    array[0] = 1;
    
    if(argc <= 2 || argc > 3){
        free(array);
        perror("\nError en el paso de parametros\n");
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
    
    /*Creating the semaphores for mutual exclusion*/
    if(Crear_Semaforo(key, 1, &semid) == ERROR){
        free(array);
        perror("\nError al crear semaforos \n");
        exit(EXIT_FAILURE);
    }
            
    /*Inicializating semaphore for*/
    if(Inicializar_Semaforo(semid, array) == ERROR){
        free(array);
        perror("\nError al inicializar semaforos\n");
        exit(EXIT_FAILURE);
    }
    
    /*Creating the key for the message queue*/
    clave = ftok(FILEKEY, N);
    if (clave == (key_t) -1){
        free(array);
        perror("\nError al obtener clave para cola mensajes\n");
        exit(EXIT_FAILURE);
    }

    /*Creating the message queue*/
    id_cola = msgget (clave, 0600 | IPC_CREAT);
    if (id_cola == -1){
        free(array);
        perror("\nError al obtener identificador para cola mensajes\n");
        exit(EXIT_FAILURE);
    }
    printf("Cola y semaforos creados");
    
    /*Creating Process A*/
    pid = fork();
    if(pid < 0){
        perror("\nError en el fork\n");
        free(array);
        exit(EXIT_FAILURE);
    }
    else if(pid != 0){
        printf("Padre");    
    }
    
    /*Process A*/
    else if(pid == 0){
        /*Creating Process B*/
        pid = fork();
        if(pid < 0){
            perror("\nError en el fork\n");
            free(array);
            exit(EXIT_FAILURE);
        }
        
        else if(pid != 0){
           Down_Semaforo(semid, 0, SEM_UNDO);
        
            /*Opening the file for reading*/
            fp = fopen(argv[1], "r+");
            if(fp == NULL){
                free(array);
                fclose(fp);
                perror("\nError en la apertura del fichero, proceso A\n");
                exit(EXIT_FAILURE);
            }
            
            /*Inicializating al the characters from mensaje to 0*/
            memset(&mensaje, 0, sizeof(mensaje));/*bzero();*/
            
            /*Reading the file until the end of it*/
            while(!feof(fp)){
                fread(mensaje.text, sizeof(char), TAM_MAX, fp);
                mensaje.id = 1;
                /*Introducing messages to the queue*/
                if(msgsnd(id_cola, &mensaje, sizeof(mensaje) - sizeof(long), 0) == -1){
                    fclose(fp);
                    perror("\nError al introducir datos a la cola, proceso A\n");
                    exit(EXIT_FAILURE);
                }
            }
            
            fclose(fp);
            printf("\nTrabajo del proceso A completado");
            Up_Semaforo(semid, 0, SEM_UNDO); 
        }
        
        /*Process B*/
        else{
            /*Creating Process C*/
            pid = fork();
            if(pid < 0){
                perror("\nError en el fork\n");
                free(array);
                exit(EXIT_FAILURE);
            }
            
            /*Process B*/
            else if(pid != 0){
                Down_Semaforo(semid, 0, SEM_UNDO);
                /*Changing all characters of the queue*/
                if(msgrcv(id_cola, &mensaje, sizeof(mensaje) - sizeof(long), 1, 0) == E2BIG){
                    free(array);
                    perror("\nError al leer datos de la cola, proceso B\n");
                    exit(EXIT_FAILURE);
                }
                
                    
                /*Loops for changing the char to the next one*/
                j = 0;
                while(mensaje.text[j] != '\0'){
                    if(mensaje.text[j] == 'Z'){
                        mensaje.text[j] = 'A';
                        mensaje.id = 2;
                        j++;
                    }
                    
                    else if(mensaje.text[j] >= 'A' || mensaje.text[j] < 'Z'){
                        mensaje.text[j]++;
                        mensaje.id = 2;
                        j++;
                    }
                    
                    else if(mensaje.text[j] == 'z'){
                        mensaje.text[j] = 'a';
                        mensaje.id = 2;
                        j++;
                    }
                    
                    else if(mensaje.text[j] >= 'a' || mensaje.text[j] < 'z'){
                        mensaje.text[j]++;
                        mensaje.id = 2;
                        j++;
                    }
                }
                    
                /*Introducing modify messages to the queue*/
                if(msgsnd (id_cola, &mensaje, sizeof(mensaje) - sizeof(long), IPC_NOWAIT) == EAGAIN){
                    free(array);
                    perror("\nError al introducir datos a la cola, proceso B\n");
                    exit(EXIT_FAILURE);
                }
                
                printf("\nTrabajo del proceso B completado");
                Up_Semaforo(semid, 0, SEM_UNDO);
            }
            
            /*Process C*/
            else{
                Down_Semaforo(semid, 0, SEM_UNDO);
                /*Opening the second file*/
                fp = fopen(argv[2], "w+");
                if(fp == NULL){
                    free(array);
                    fclose(fp);
                    perror("\nError en la apertura del fichero2, proceso C\n");
                    exit(EXIT_FAILURE);
                }
                
                /*Rediang modified queue*/
                if(msgrcv(id_cola, &mensaje, sizeof(mensaje) - sizeof(long), 2, 0) == E2BIG){
                    free(array);
                    fclose(fp);
                    perror("\nError al leer datos de la cola, proceso C\n");
                    exit(EXIT_FAILURE);
                }
                    
                /*Writing in the second file*/
                if(fprintf(fp, "%s", mensaje.text) <= 0){
                    free(array);
                    fclose(fp);
                    perror("\nError al escribir datos en el fichero2, proceso C\n");
                    exit(EXIT_FAILURE);
                }
                
        
                fclose(fp);        
                free(array);
                printf("\nTrabajo del proceso C completado");
                Up_Semaforo(semid, 0, SEM_UNDO);
                exit(EXIT_SUCCESS);
            }
            
            free(array);
            /*Waiting all the son processes of process B*/
            while(wait(NULL)>0);
            exit(EXIT_SUCCESS);
        }
        
        free(array);
        /*Waiting all the son processes of process A*/
        while(wait(NULL)>0);
        exit(EXIT_SUCCESS);
    }
    
    /*Waiting all the son processes*/
    while(wait(NULL)>0);
    
    /*Erasing queue*/
    if(msgctl(id_cola, IPC_RMID, (struct msqid_ds *)NULL) == -1){
        free(array);
        perror("\nPuta vida tete\n");
        exit(EXIT_FAILURE);
    } 
    
    printf("\nNiceee!!");
            
    /*Destroying the semaphore*/
    Borrar_Semaforo(semid);
    free(array);
    
    exit(EXIT_SUCCESS);
}