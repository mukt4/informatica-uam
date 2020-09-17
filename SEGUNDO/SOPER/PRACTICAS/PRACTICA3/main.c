#define _POSIX_SOURCE   /*PARA LA UTILIZACION DEL COMANDO KILL*/
#define _BSD_SOURCE     /*PARA LA UTILIZACION DEL COMANDO USLEPP*/

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
#include "aleatorio.h"
#include "seniales.h"

#define ESPERA 30
#define FILEKEY "/bin/cat"
#define KEY 2357
#define EXTRA 0     /*PARA LA CREACION DE SEMAFOROS EXTRA ADEMAS DE LAS VENTANILLAS*/

int contador = 0;   /*VARIABLE GLOBAL PARA CONTROLAR QUE TODOS LOS CABALLOS HAN REALIZADO SU TIRADA*/

int main() {
    int nCaballos, longitud, nApostantes, nVentanillas, i, pipe_status;
    int pid, pidMonitor, pidGestor, pidApostador, semid;
    int** tuberia;
    int* inicializacion;
    int* pidCaballos;
    double dinero;
    sigset_t mascaraProcesos[4];
    sigset_t* mascaraCaballos;
    
    
    
    /**********************************
     *      RECOPILACION DE DATOS    *
     * *******************************/
    
    /*NUMERO DE CABALLOS EN LA CARRERA*/
    do {
        printf("Introduzca el numero de caballos que van a participar en la carrera(Como maximo 10): ");
        scanf("%d", &nCaballos);
    } while(nCaballos > 10);
    
    /*LONGITUD DE LA CARRERA*/
    printf("\nIntroduzca la longitud de la carrera: ");
    scanf("%d", &longitud);
   
    /*APOSTANTES DE LA CARRERA*/ 
    do {
        printf("\nIntroduzca el numero de apostantes en la carrera(Como maximo 100): ");
        scanf("%d", &nApostantes);
    } while(nApostantes > 100);
    
    /*VENTANILLAS PARA GESTIONAR LAS APUESTAS*/
    printf("\nIntroduzca el numero de ventanillas para gestionar las apuestas: ");
    scanf("%d", nVentanillas);
    
    /*DINERO DISPONIBLE PARA LOS APOSTANTES*/
    PRINTF("\nIntroduzca el dinero disponinle para los apostantes: ");
    scanf("%lf", &dinero);
    
    /*******************************************
     * INICIALIZACION DE TUBERIAS Y SEMAFOROS  *
     * ****************************************/
     
    /*INICIALIZACION DE TUBERIAS*/
    /*LA PRIMERA MITAD PARA QUE EL PRINCIPAL INFORME A LOS CABALLOS*/
    /*LA SEGUNDA MITADA PARA QUE LOS CABALLOS INFORMEN AL PROCESO PRINCIPAL*/
    /*0 PARA LEER Y 1 PARA ESCRIBIR*/
    
    tuberia = (int**)malloc((nCaballos * 2) * sizeof(int*));
    
    for(i = 0; i < nCaballos; i++){
        tuberia[i] = (int*)malloc(2*sizeof(int));
    }
    
    for(i = 0; i < (nCaballos * 2); i++){
        pipe_status = pipe(tuberia[i]);
        if(pipe_status == -1){
            perror("Error en la creacion de la tuberia %d", i);
            exit(EXIT_FAILURE);
        }
    }
     
    /*INICIALIZACION Y CREACION DE SEMAFOROS*/
    
    semid = semget(SEMKEY, (nVentanillas + EXTRA), IPC_CREAT | IPC_EXCL | SHM_R | SHM_W);
    
    if((sem_id == -1) && (errno == EEXIST)) {
        sem_id = semget(SEMKEY, (nVentanillas + EXTRA), SHM_R|SHM_W);
    }
    if(sem_id == -1) {
        perror("Error en la inicializacion de la semilla para la creacion de los semaforos");
        exit(EXIT_FAILURE);
    }
    
    if(Crear_Semaforo(SEMKEY,nVentanillas + EXTRA, &semid) == ERROR){
        perror("Error en la creacion de los semaforos");
        exit(EXIT_FAILURE);
    }
    
    inicializacion = (int*)malloc((nVentanillas + EXTRA) * sizeof(int));
    
    for(i = 0; i < (nVentanillas + EXTRA); i++){
        inicializacion[i] = 1;
    }
    
    if(Inicializar_Semaforo(semid, inicializacion) == ERROR){
        perror("Error en la inicializacion de los semaforos");
        exit(EXIT_FAILURE);
    }
    
    /*RESERVA DE MEMORIA PARA LAS MASCARAS DE LOS CABALLOS*/
    
    mascaraCaballos = (sigset_t*)malloc(nCaballos * sizeof(sigset_t));
    
    /*RESERVA DE MEMORIA PARA GUARDAR LOS PIDS DE LOS CABALLOS*/
    
    pidCaballos = (int*)malloc(nCaballos * sizeof(int));
    
    /*************************************
     *  COMIENZO DEL PROGRAMA PRINCIPAL  *
    *************************************/
    
    pid = fork();
    
    if(pid < 0) {
        perror("Error en el fork monitor");
        exit(EXIT_FAILURE);
    }
    else if(pid) {
        pidMonitor = pid;
        pid = fork();
        
        if(pid < 0) {
            perror("Error en el fork gestor de apuestas");
            exit(EXIT_FAILURE);
        } 
        else if(pid) {
            pidGestor = pid;
            pid = fork();
            
            if(pid < 0) {
                perror("Error en el fork apostador");    
            }
            else if(pid){
                pidApostador = pid;
                
                /*CREACION DE CABALLOS*/
                
                for(i = 0; i < nCaballos; i++){
                    pid = fork();
                
                    if(pid < 0) {
                        perror("\nError en el fork de creacion del caballo %d", i);
                        exit(EXIT_FAILURE);
                    } 
                    else if(pid == 0) {
                        /*******************************
                        *PROCESO QUE ACTUA COMO CABALLO*
                        *******************************/
                        
                        /*INICIALIZACION TUBERIAS*/
        
                        /*CIERRE DEL DESCRIPTOR DE ESCRITURA*/
                        close(tuberia[i][1])
                        /*CIERRE DEL DESCRIPTOR DE LECTURA*/
                        close(tuberia[i + nCaballos][0]);
                        
                        /*INICIALIZACION DE LA MASCARA PARA LOS CABALLOS*/
                        
                        /*BLOQUEO DE TODAS LAS SENIALES*/
                        sigfillset(&mascaraCaballos[i]);
                        /*DESBLOQUEO DE LA SENIAL SIGALRM*/
                        sigdelset(&mascaraCaballos[i], SIGALRM);
                        /*DESBLOQUEO DE LA SENIAL SIGKILL*/
                        sigdelset(&mascaraCaballos[i], SIGKILL);
                        
                        /*ESPERA HASTA QUE EL PROCESO PRINCIPAL DE LA SALIDA*/
                        if(signal(SIGALRM, manejador_SIGALALRM) == SIG_ERR){
                            perror("Error en el manejador de espera de los caballos");
                            exit(EXIT_FAILURE);
                        }
                        pause();
                    }
                    else{
                        pidCaballos[i] = pid;
                    }
                }
                /************************************************
                *PROCESO PRINCIPAL DESPUES DE CREAR LOS CABALLOS*
                ************************************************/
                
                /*INICIALIZACION DE LA MASCARA PARA EL PROGRAMA PRINCIPAL*/
                
                /*BLOQUEO DE TODAS LAS SENIALES*/
                sigfillset(&mascaraProcesos[0]);
                /*DESBLOQUEO DE LA SENIAL SIGINT*/
                sigdelset(&mascaraProcesos[0], SIGINT);
                /*DESBLOQUEO DE LA SENIAL SIGTRAP*/
                sigdelset(&mascaraProcesos[0], SIGTRAP);
                
                /*MANEJADOR DE LA SENIAL CTRL + C*/
                if(signal(SIGINT, manejador_SIGINT) == SIG_ERR){
                    perror("Error en el manejador de CTR + C");
                    exit(EXIT_FAILURE);
                }
                
                /*BUCLE INFINITO QUE NO TERMINARA HASTA QUE O BIEN TERMINE LA CARRERA O BIEN EL USUARIO INTRODUZCA CTRL + C*/
                
                while(1){
                    
                    /*ESPERA A QUE TERMINEN LAS APUESTAS*/
                    
                    if(signal(SIUSR1, manejador_SIGUSR1) == SIG_ERR){
                        perror("Error en el manejador de la senial de espera de tiempo");
                        exit(EXIT_FAILURE);
                    }
                    pause();
                
                    for(i = 0; i < nCaballos; i++){
                        kill(pidCaballos[i], SIGALRM);
                    }
                
                    /*ESPERA A QUE LOS CABALLOS REALICEN SUS RESPECTIVAS TIRADAS*/
                    if(signal(SIGTRAP, manejador_SIGTRAP) == SIG_ERR){
                        perror("Error en el manejador de la senial de que el caballo a realizado su tirada");
                        exit(EXIT_FAILURE);
                    }
                    
                    /*BUCLE INFINITO DE ESPERA HASTA QUE LOS CABALLOS TERMINEN*/
                    while(contador != nCaballos){
                        
                    }
                    
                    /*LECTURA DE LAS TIRADAS DE LOS CABALLOS Y COLOCACION DE LOS MISMOS*/
                }
                
            }
            else{
                /******************
                *PROCESO APOSTADOR*
                ******************/            
            }
            
            
            
        }
        else {
            /*************************************************
            *PROCESO QUE SE ENCARGA DE GESTIONAR LAS APUESTAS*
            *************************************************/
            
        }
    }
    else {
        /****************
        *PROCESO MONITOR*
        ****************/
        
        /*CUENTA ATRAS DE COMIENZO DE LA CARRERA*/
        
        for(i = 30; i > 0; i++){
            printf("\nCUENTA ATRAS: %d", i);
            
            /*MOSTRAR COTIZACION DE LAS APUESTAS*/
            
            printf("\nAPUESTAS...");
            
            /*ESPERA DE 1 SEGUNDO*/
            
            usleep(1000000);
        }
        printf("\nCOMIENZA LA CARRERA!!");
        
        /*MANDA SENIAL AL PROCESO PRINCIPAL, A LOS CABALLOS, A LOS APOSTADORES Y AL GESTOR DE APUESTAS*/
    } 
}


/*ESTE MANEJADOR SERA IMPLEMENTADO EN EL MAIN PARA QUE TENGA CONTROL SOBRE LA VARIABLE CONTADOR*/
void manejador_SIGTRAP(int sig){
    printf("Un caballo a finalizado su tirada");
    contador++;
}