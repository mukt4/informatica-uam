/*#define _BSD_SOURCE
#define _POSIX_SOURCE*/
#define _POSIX_SOURCE/*kill*/
#define _BSD_SOURCE/*usleep*/

#include <sys/wait.h>
#include <time.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <sys/shm.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "semaforos.h"

#define FILEKEY "/bin/cat"
#define KEY 2357
#define N_SEMAFOROS 2 /*UNO DE LOS SEMAFOROS PARA LAS CAJAS Y OTRO PARA LAS CUENTAS*/
#define TAM 500

#define CAJAS 2

int numAleatorio();
int segsAleatorios();

void manejador_SIGUSR1(int sig);
void manejador_SIGUSR2(int sig);

int main(){
    FILE* pf;
    FILE* caja;
    int i, j, pid, status;
    char nombre[TAM], nombre_caja[TAM];
    char aux[TAM];
    int num;
    int key = ftok(FILEKEY, KEY);
    int cuenta = 0, cuenta_global = 0;
    int semid;
    
    /*
    * Declaración de variables
    */
    union semun {
        int val;
        struct semid_ds *semstat;
        unsigned short *array;
    } arg;
    
    /*CREAMOS DOS SEMAFOROS, UNO PARA LAS CAJAS Y OTRO PARA LAS CUENTAS*/
    if(Crear_Semaforo(key, N_SEMAFOROS, &semid) == ERROR)
        perror("Error al crear semaforos");
        
    arg.array = (unsigned short *)malloc(sizeof(short)*CAJAS);
    
    for( i = 0; i < CAJAS; i++){
        arg.array[i] = 1;
    }
    
    if(Inicializar_Semaforo(semid, arg.array) == ERROR)
        perror("Error al inicializar semaforos");

    for( i = 0; i < CAJAS; i++){
        pid = fork();
        
        if(pid < 0){
            perror("Error en el fork");
            exit(EXIT_FAILURE);
        }
        
        /*PROCESO DEL PADRE EN EL CUAL GENERA OPERACIONES ALEATORIAS*/
        else if(pid){
            printf("\nCarga %d\n", i);
            /*BAJAMOS SEMAFORO DE LA CAJAS PARA RELLENAR DATOS*/
            Down_Semaforo(semid, 0, SEM_UNDO);
            /*BAJAMOS EL SEMEAFORO DE LA CUENTA PARA INCIALIZAR*/
            Down_Semaforo(semid, 1, SEM_UNDO);
            bzero(nombre_caja, TAM);
            bzero(nombre, TAM);
            sprintf(nombre_caja, "caja%d.txt",i);
            sprintf(nombre, "clientesCaja%d.txt", i);
            pf = fopen(nombre, "w");
            caja = fopen(nombre_caja, "w");
            /*RELLENANDO OPERACIONES*/
            /*CAMBIAR OPERACIONES A 50*/
            for(j = 0; j < 50; j++){
                fprintf(pf, "%d\n", numAleatorio());
            }
            fprintf(caja, "0");
            fclose(caja);
            fclose(pf);
            Up_Semaforo(semid, 0, SEM_UNDO);
            Up_Semaforo(semid, 1, SEM_UNDO);
        }
     
        /*PROCESO DEL HIJO EN EL CUAL VE SUS OPERACIONES*/
        else{
            printf("\nLectura %d\n",i);
            /*BAJAMOS SEMAFORO DE LAS CAJAS PARA LEER DATOS*/
            bzero(nombre, TAM);
            sprintf(nombre, "clientesCaja%d.txt", i);
            sprintf(nombre_caja, "caja%d.txt",i);
            Down_Semaforo(semid, 0, SEM_UNDO);
            pf = fopen(nombre, "r");
            if(pf == NULL){
                perror("Error en la lectura de fichero");
                exit(EXIT_FAILURE);
            }
            while(!feof(pf)){
                /*CAMBIAR A 1000000 PARA QUE ESTE EN SEGUNDOS*/
                usleep(segsAleatorios());
                /*BAJAMOS EL SEMAFORO DE LECTURA DE CAJAS*/
                fgets(aux, TAM, pf);
                num = atoi(aux);
                /*BAJAMOS EL SEMAFORO DE ESCRITURA EN CUENTAS*/
                Down_Semaforo(semid, 1, SEM_UNDO);
                caja = fopen(nombre_caja, "rw");
                if(caja == NULL){
                    perror("ERROR EN EL NOMBRE DE FICHERO");
                    exit(EXIT_FAILURE);
                }
                fseek(caja, 0L, SEEK_SET);
                fscanf(caja, "%d", &cuenta);
                fclose(caja);
                caja = fopen(nombre_caja, "w");
                fseek(caja, 0L, SEEK_SET);
                cuenta += num;
                fprintf(caja, "%d", cuenta);
                fclose(caja);
                Up_Semaforo(semid, 1, SEM_UNDO);
                /*SI LA CUENTA SUPERA 1000 AVISA AL PROCESO PADRE PARA QUE LE RETIRE EL DINERO*/
                if(cuenta > 1000){
                    /*FALTA ENVIAR LA SENIAL Y RECIBIRLA EN EL PADRE*/
                }
            }
            fclose(pf);
            /*LEVANTAMOS EL SEMAFORO DE LECTURA DE CAJAS*/
            Up_Semaforo(semid, 0, SEM_UNDO);
            exit(EXIT_SUCCESS);
        }
    }
    wait(&status);
    wait(&status);
    for(i = 0; i < CAJAS; i++){
        bzero(nombre_caja, TAM);
        sprintf(nombre_caja, "caja%d.txt",i);
        caja = fopen(nombre_caja, "r");
        fscanf(caja, "%d", &cuenta);
        cuenta_global += cuenta;
        fclose(caja);
    }
    printf("\nCUENTA GLOBAL DESPUES DE TODO EL PROCESO %d\n", cuenta_global);
    Borrar_Semaforo(semid);
    exit(EXIT_SUCCESS);
}

int numAleatorio(){
    
    int numero;
    
    numero = rand() % 301;
    if(numero < 0 || numero > 300){
        return -1;
    }
    
    return numero;
}

int segsAleatorios(){
    
    int numero;
    
    numero = rand() % 4;
    if(numero < 0 || numero > 4){
        return -1;
    }
    
    return numero+1;
}

void manejador_SIGUSR1 (int sig){
    printf("\nLLegaste a 1000 euros, en cuanto sea posible se le retiraran 900 de la caja\n");
    
}

void manejador_SIGUSR2(int sig){
    printf("\nSe termino tu trabajo\n");
    signal(SIGUSR2, SIG_DFL);
}