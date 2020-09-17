#define _POSIX_SOURCE   /*PARA LA UTILIZACION DEL COMANDO KILL*/
#define _BSD_SOURCE     /*PARA LA UTILIZACION DEL COMANDO USLEPP*/

#include <sys/wait.h>
#include <time.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>
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

#include "semaforos.h"
#include "aleatorio.h"
#include "seniales.h"
#include "gestor.h"
#include "monitor.h"

#define ESPERA 10
#define ESPERA_FINAL 15
#define FILEKEY "/bin/cat"
#define KEY 1111

/*********************************************************************************
 * SEMAFOROS
 * 0- SEMAFORO DE ESPERA PARA EL PROCESO PRINCIPAL HASTA QUE LOS CABALLOS TERMINEN
 * 1- MUTEX PARA ACCEDER A MEMORIA COMPARTIDA
 * 2- MUTEX PARA ACCEDER A LA COLA DE MENSAJES
 * 3- MUTEX PARA CONTROLAR EL INTERBLOQUEO EN LA COLA DE MENSAJES
 * 4- SEMAFORO PARA CONTROLAR PROCESO MONITOR Y PROGRAMA PRINCIPAL
 * 5->N - SEMAFOROS DE LOS CABALLOS PARA QUE EL PROCESO PRINCIPAL LES DEJE CORRER
 * *******************************************************************************/
 
 /**********************************************
  * MASCARAS DE SENIALES
  * 0- PROCESO PRINCIPAL
  * 1- PROCESO APOSTADOR
  * 2- PROCESO MONITOR
  * 3- PROCESO GESTOR
  * 4- PROCESO CABALLO
  * ********************************************/

int main(int argc, char** argv) {
    int nCaballos, longitud, nApostantes, nVentanillas, i, j, pipe_status;
    int pid, pidMonitor, pidGestor, semid, idCompartida, idCola;
    int tipo_tirada, lanzamiento;
    int primero, ultimo;
    int key;
    int idApuesta = 0;
    int** tuberiaPrincipal;
    int** tuberiaCaballos;
    int* pidCaballos;
    int* pidApostadores;
    int* posicion_aux;
    int* id_hilo;
    Gestion* infoVentanilla;
    pthread_t* ventanilla;
    unsigned short* inicializar;
    double dinero, dineroDisponible;
    sigset_t mascaraProcesos[5];
    char readbuffer[TAM_NOMBRE];
    char writebuffer[TAM_NOMBRE];
    char nombreApostador[TAM_NOMBRE];
    Informacion* info;
    Mensaje mensaje;

    /**********************************
     *      RECOPILACION DE DATOS    *
     * *******************************/
    
    if(argc != 6){
		printf("Por favor, ejecutelo de la siguiente manera: ./carrera_caballos <caballos> <longitud> <apostadores> <ventanillas> <dinero>\n");
		return -1;
	}
	
	nCaballos = atoi(argv[1]);
	longitud = atoi(argv[2]);
	nApostantes = atoi(argv[3]);
	nVentanillas = atoi(argv[4]);
	dinero = strtod(argv[5],NULL);
	
	if(nApostantes > MAX_APOSTANTES){
	    printf("Error en los campos introducidos");
	    exit(EXIT_FAILURE);
	}
	if(nCaballos > MAX_CABALLOS){
	    printf("Error en los campos introducidos");
	    exit(EXIT_FAILURE);
	}
    
    
    /***************************************************************
     * INICIALIZACION DE TUBERIAS, SEMAFOROS Y MEMORIA COMPARTIDA  *
     * ************************************************************/
     
    /*INICIALIZACION DE TUBERIAS*/
    /*LA PRIMERA MITAD PARA QUE EL PRINCIPAL INFORME A LOS CABALLOS*/
    /*LA SEGUNDA MITADA PARA QUE LOS CABALLOS INFORMEN AL PROCESO PRINCIPAL*/
    /*0 PARA LEER Y 1 PARA ESCRIBIR*/
    
    tuberiaPrincipal = (int**)malloc(nCaballos * sizeof(int*));
    
    for(i = 0; i < nCaballos; i++){
        tuberiaPrincipal[i] = (int*)malloc(2 * sizeof(int));
    }
    
    for(i = 0; i < nCaballos; i++){
        pipe_status = pipe(tuberiaPrincipal[i]);
        if(pipe_status == -1){
            perror("Error en la creacion de la tuberias");
            exit(EXIT_FAILURE);
        }
    }
    
    tuberiaCaballos = (int**)malloc(nCaballos * sizeof(int*));
    
    for(i = 0; i < nCaballos; i++){
        tuberiaCaballos[i] = (int*)malloc(2 * sizeof(int));
    }
    
    for(i = 0; i < nCaballos; i++){
        pipe_status = pipe(tuberiaCaballos[i]);
        if(pipe_status == -1){
            perror("Error en la creacion de la tuberias");
            exit(EXIT_FAILURE);
        }
    }
    
    /*RESERVA DE MEMORIA PARA GUARDAR LOS IDS DE LOS HILOS QUE ACTUAN COMO VENTANILLAS*/
    id_hilo = (int*)malloc(nVentanillas * sizeof(int));
    
    /*RESERVA DE MEMORIA PARA GUARDAR LOS PIDS DE LOS CABALLOS*/
    
    pidCaballos = (int*)malloc(nCaballos * sizeof(int));
    
    /*RESERVA DE MEMORIA PARA GUARDAR LOS PIDS DE LOS APOSTADORES*/
    
    pidApostadores = (int*)malloc(nApostantes * sizeof(int));
    
    /*INICIALIZACION DE LA MASCARA DEL PROCESO PRINCIPAL*/
    sigfillset(&mascaraProcesos[0]);
    sigdelset(&mascaraProcesos[0], SIGALRM);
    
    /*INICIALIZACION DE LA MASCARA DE LOS PROCESOS APOSTADORES*/
    sigfillset(&mascaraProcesos[1]);
    sigdelset(&mascaraProcesos[1], SIGUSR2);
    
    /*INICIALIZACION DE LA MASCARA DEL PROCESO GESTOR DE APUESTAS*/
    sigfillset(&mascaraProcesos[2]);
    sigdelset(&mascaraProcesos[2], SIGTERM);
    
    /*INICIALIZACION DE LA MASCARA DE LOS CABALLOS*/
    sigfillset(&mascaraProcesos[4]);
    sigdelset(&mascaraProcesos[4], SIGUSR1);
    
    /*INICIALIZACION DE LOS SEMADOROS*/
    
    key = ftok(FILEKEY, KEY);
    
    if(Crear_Semaforo(key, 5 + nCaballos, &semid) == ERROR){
        perror("Error en la creacion del semaforo");
        exit(EXIT_FAILURE);
    }
       
    /*RESERVA DE MEMORIA PARA EL ARRAY DE INCIALIZACION*/
    
    inicializar = (unsigned short*)malloc((5 + nCaballos) * sizeof(unsigned short));
    for(i = 0; i < 5 + nCaballos; i++){
        inicializar[i] = 0;
    }
    
    if(Inicializar_Semaforo(semid, inicializar) == ERROR){
        perror("Error en la inicializacion del semaforo");
        exit(EXIT_FAILURE);
    }
    
    /*INCIALIZACION DEL MUTEX DE LA COLA DE MENSAJES*/
    Up_Semaforo(semid, 2, SEM_UNDO);
    
     /*INICIALIZACION DEL SEMAFORO MUTEX PARA LA ZONA DE MEMORIA COMPARTIDA*/
    Up_Semaforo(semid, 1, SEM_UNDO);
    
    /*INICIALIZACION DE ZONA DE MEMORIA COMPARTIDA*/
    info = (Informacion*)malloc(sizeof(Informacion));
    
    idCompartida = shmget(key, sizeof(info), IPC_CREAT | SHM_W | SHM_R);
    
    if(idCompartida == -1){
        perror("Error en la inicializacion de la memoria compartida");
        exit(EXIT_FAILURE);
    }
    
    /*INICIALIZAMOS LA MEMORIA COMPARTIDA*/
    info = shmat(idCompartida, (char*)0, SHM_W | SHM_R);
                
    if(info == NULL){
        perror("Error en la conexion de memoria compartida");
        for(i = 0; i < nCaballos; i++){
                kill(pidCaballos[i], SIGUSR1);
            }
            exit(EXIT_FAILURE);
    }
        
    for(i = 0; i < nCaballos; i++){
        info->posicionCaballos[i] = 0;
        info->dineroApostado[i] = 1;
        info->cotizacionCaballo[i] = 0;
    }
    info->totalDinero = nCaballos;
    info->idMensaje = 1;
    info->ganancia[i] = 0;
        
    for(i = 0; i < nApostantes; i++){
        info->totalDineroApostador[i] = dinero;
    }
    
    for(i = 0; i < nApostantes; i++){
        for(j = 0; j < MAX_APUESTAS; j++){
            mensaje = info->apuestas[i][j];
            mensaje.id = 0;
        }
    }
            
    shmdt(info);
    
    
    /*RESERVAMOS MEMORIA PARA TENER CONTRDOL DE LA POSICION DE LOS CABALLOS*/
    posicion_aux = (int*)malloc(nCaballos * sizeof(int));
    
    /*RESERVA DE MEMORIA PARA LOS HILOS QUE ACTUAN COMO VENTANILLAS*/
    ventanilla = (pthread_t*)malloc(nVentanillas*sizeof(pthread_t));
    
    /*INICIALIZACION DE LA COLA DE MENSAJES PARA GESTIONAR LAS APUESTAS*/
    idCola = msgget (key, IPC_CREAT | 0660);
    
    if(idCola == -1){
        perror("Error en la creacion de la cola de mensajes");
        exit(EXIT_FAILURE);
    }
    
    /*INICIALIZACION DE LA ESTRUCTURA QUE CONTIENE INFORMACION RELEVANTE PARA LOS HILOS*/
    infoVentanilla = (Gestion*)malloc(sizeof(Gestion));
    infoVentanilla->id_cola = idCola;
    infoVentanilla->id_semaforo = semid;
    infoVentanilla->id_memoria = idCompartida;
    
    
    /*************************************
     *  COMIENZO DEL PROGRAMA PRINCIPAL  *
    *************************************/
    
    /*CREACION DEL PROCESO MONITOR*/
    
    pid = fork();
    
    if(pid < 0) {
        perror("Error en el fork monitor");
        exit(EXIT_FAILURE);
    }
    else if(pid) {
        
        pidMonitor = pid;
        
         /*CREACION DEL PROCESO GESTOR DE APUESTAS*/
        pid = fork();
        
        if(pid < 0) {
            perror("Error en el fork gestor de apuestas");
            exit(EXIT_FAILURE);
        } 
        else if(pid) {
            pidGestor = pid;
            
            /*CREACION DE LOS PROCESOS APOSTANTES*/
            
            for(i = 0; i < nApostantes; i++){
                pid = fork();
                
                if(pid < 0) {
                    perror("Error en el fork apostador");    
                }
                else if(pid == 0){
                    /******************
                    *PROCESO APOSTADOR*
                    ******************/
                    
                    /*DECLARACION DEL MANEJADOR QUE SE ENCARGA DE RECIBIR LA SENIAL DE FIN DE TIEMPO DE APUESTAS*/
                    signal(SIGTERM, manejador_SIGTERM);
                    bzero(nombreApostador, TAM_NOMBRE);
                    sprintf(nombreApostador, "Apostador-%d", i + 1);
                    dineroDisponible = dinero;
                    
                    while(1){
                        /*SE SETEA LA APUESTA*/
                        mensaje.n_caballo = caballo_aleatorio(nCaballos);
                        mensaje.apuesta = dinero_aleatorio(dineroDisponible);
                        mensaje.idApostante = i;
                        strcpy(mensaje.nombre, nombreApostador);
                        dineroDisponible = dineroDisponible - mensaje.apuesta;
                        /*SE BAJA EL MUTEX DE LA MEMORIA COMPARTIDA*/
                        Down_Semaforo(semid, 1, SEM_UNDO);
                        info = shmat(idCompartida, (char*)0, SHM_W | SHM_R);
                        mensaje.id = info->idMensaje;
                        info->idMensaje++;
                        shmdt(info);
                        Up_Semaforo(semid, 1, SEM_UNDO);
                        mensaje.nApuesta = idApuesta;
                        idApuesta++;
                        /*SE ENVIA LA APUESTA*/
                        Down_Semaforo(semid, 2, SEM_UNDO);
                        msgsnd(idCola, (struct msgbuf *) &mensaje, sizeof(Mensaje) - sizeof(long), IPC_NOWAIT);
                        Up_Semaforo(semid, 2, SEM_UNDO);
        
                        /*LEVANTO EL SEMAFORO PARA CONTROLAR EL INTERBLOQUEO DE LA COLA DE MENSAJES*/
                        Up_Semaforo(semid, 3, SEM_UNDO);
                        
                        if(dineroDisponible == 0){
                            sigsuspend(&mascaraProcesos[1]);
                            pause();
                        }
                        
                        /*ESPERA DE UN SEGUNDO PARA REALIZAR OTRA APUESTA*/
                        usleep(1000000);
                    }
                }
                else{
                    pidApostadores[i] = pid;
                }
            }
                
            /*CREACION DE CABALLOS*/
                
            for(i = 0; i < nCaballos; i++){
                pid = fork();
                
                if(pid < 0) {
                    perror("\nError en el fork de creacion de caballos");
                    exit(EXIT_FAILURE);
                }
                else if(pid == 0){
                    /*******************************
                    *PROCESO QUE ACTUA COMO CABALLO*
                    *******************************/
                    /*MANEJADOR QUE CONTROLA EL FIN DE PROGRAMA DE LOS CABALLOS*/
                    signal(SIGUSR1, manejador_SIGUSR1);
                        
                    while(1){ 
                        Down_Semaforo(semid, 5 + i, SEM_UNDO);
                        bzero(readbuffer, TAM_NOMBRE);
                        read(tuberiaPrincipal[i][0], readbuffer, TAM_NOMBRE);
                        tipo_tirada = atoi(readbuffer);
                        switch(tipo_tirada){
                            case ESTANDAR:
                                lanzamiento = tirada_normal();
                                break;
                            case GANADORA:
                                lanzamiento = tirada_ganadora();
                                break;
                            case REMONTADORA:
                                lanzamiento = tirada_remontadora();
                                break;
                            default:
                                perror("Tipo de tirada incorrecta");
                                exit(EXIT_FAILURE);
                        }
                        bzero(writebuffer, TAM_NOMBRE);
                        sprintf(writebuffer, "%d", lanzamiento);
                        write(tuberiaCaballos[i][1], writebuffer, strlen(writebuffer));
                        Up_Semaforo(semid, 0, SEM_UNDO);
                    }
                }
                else{
                    pidCaballos[i] = pid;
                }
            }
            /************************************************
            *PROCESO PRINCIPAL DESPUES DE CREAR LOS CABALLOS*
            ************************************************/
          
            /*INICIALIZACION DE LOS CAPTURADORES*/
            signal(SIGALRM, manejador_SIGALRM);
            
            /*INCIALIZACION DE LAS TUBERIAS DEL PROCESO PRINCIPAL*/
            for(i = 0; i < nCaballos; i++){
                close(tuberiaPrincipal[i][0]);
            }
                
            for(i = 0; i < nCaballos; i++){
                close(tuberiaCaballos[i][1]);
            }
                        
            /*ESPERA HASTA QUE EL PROCESO MONITOR DE LA SALIDA*/
            sigsuspend(&mascaraProcesos[0]);
            
            /*AVISAMOS A LOS PROCESOS APOSTADORES Y AL PROCESO GESTOR DE APUESTAS DE QUE SE HA ACDABADO EL TIEMPO PARA REALIZAR APUESTAS*/
            for(i = 0; i < nApostantes; i++){
               kill(pidApostadores[i], SIGUSR2);
            }
            
            kill(pidGestor, SIGTERM);
                
            bzero(writebuffer, TAM_NOMBRE);
            sprintf(writebuffer, "%d", ESTANDAR);
            for(i = 0; i < nCaballos; i++){
                write(tuberiaPrincipal[i][1], writebuffer, strlen(writebuffer));
            }
                
            while(1){
                /*SEMAFORO PARA CONTROLAR LA CORRECTA IMPRESION DE LA CARRERA*/
                Down_Semaforo(semid, 4, SEM_UNDO);
                /*INICIALIZAMOS LAS VARIABLES DE CONTROL DE POSICION MAYOR Y MENOR*/
                primero = 0;
                ultimo = INT_MAX;
                /*AVISAMOS A LOS CABALLOS DE QUE YA PUEDEN REALIZAR SU TIRADA*/
                for(i = 0; i < nCaballos; i++){
                    Up_Semaforo(semid, 5 + i, SEM_UNDO);
                }
                    
                /*SE ESPERA HASTA QUE TODOS LOS CABALLOS TERMINEN*/
                
                for(i = 0; i < nCaballos; i++){
                    Down_Semaforo(semid, 0, SEM_UNDO);
                }
                    
                /*BAJAMOS SEMAFORO MUTEX DE MEMORIA COMPARTIDA*/
                Down_Semaforo(semid, 1, SEM_UNDO);
                    
                /*GUARDAMOS LAS POSICIONES EN MEMORIA COMPARTIDA*/
                info = shmat(idCompartida, (char*)0, SHM_W | SHM_R);
                if(info == NULL){
                    perror("Error al inicializar la zona de memoria compartida");
                    for(i = 0; i < nCaballos; i++){
                        kill(pidCaballos[i], SIGUSR1);
                    }
                    exit(EXIT_FAILURE);
                }
                for(i = 0; i < nCaballos; i++){
                    bzero(readbuffer, TAM_NOMBRE);
                    read(tuberiaCaballos[i][0], readbuffer, TAM_NOMBRE);
                    lanzamiento = atoi(readbuffer);

                    posicion_aux[i] = info->posicionCaballos[i];
                    posicion_aux[i] += lanzamiento;
                    info->posicionCaballos[i] = posicion_aux[i];

                    if(posicion_aux[i] >= longitud){
                        for(i = 0; i < nCaballos; i++){
                            kill(pidCaballos[i], SIGUSR1);
                            shmdt(info);
                            Borrar_Semaforo(semid);
                            msgctl (idCola, IPC_RMID, (struct msqid_ds*)NULL);
                            shmctl(idCompartida, IPC_RMID,  (struct shmid_ds *)NULL);
                        }
                        kill(pidMonitor, SIGSYS);
                        system("clear");
                        imprimir_final(idCompartida, nCaballos, nApostantes, dinero);
                        usleep(ESPERA_FINAL * 1000000);
                        exit(EXIT_SUCCESS);
                    }
                    if(posicion_aux[i] < ultimo){
                        ultimo = posicion_aux[i];
                    }
                    if(posicion_aux[i] > primero){
                        primero = posicion_aux[i];
                    }
                }
                Up_Semaforo(semid, 1, SEM_UNDO);
                    
                /*ENVIAMOS A LOS CABALLOS SU TIPO DE TIRADA PARA EL PROXIMO LANZAMIENTO*/
                for(i = 0; i < nCaballos; i++){
                     bzero(writebuffer, TAM_NOMBRE);
                    if(posicion_aux[i] == primero){
                        sprintf(writebuffer, "%d", GANADORA);
                        write(tuberiaPrincipal[i][1], writebuffer, strlen(writebuffer));
                    }
                    else if(posicion_aux[i] == ultimo){
                        sprintf(writebuffer, "%d", REMONTADORA);
                        write(tuberiaPrincipal[i][1], writebuffer, strlen(writebuffer));
                    }
                    else{
                        sprintf(writebuffer, "%d", ESTANDAR);
                        write(tuberiaPrincipal[i][1], writebuffer, strlen(writebuffer));
                    }
                }
            }
        }
        else {
            signal(SIGTERM, manejador_SIGTERM);
            for(i = 0; i < nVentanillas; i++){
                id_hilo[i] = pthread_create(ventanilla, NULL , gestion ,(void *)infoVentanilla);
            }
            sigsuspend(&mascaraProcesos[2]);
            for(i = 0; i < nVentanillas; i++){
                kill(id_hilo[i], SIGTERM);
            }
            exit(EXIT_SUCCESS);
        }
    }
    else {
        /****************
        *PROCESO MONITOR*
        ****************/
        printf("\nCUENTA ATRAS\n");
        
        for(i = ESPERA; i > 0; i--){
            system("clear");
            printf("\nTiempo restante: %d\n", i);
            imprimir_cotizaciones(idCompartida, nCaballos);
            usleep(1000000);
        }
        Up_Semaforo(semid, 4, SEM_UNDO);
        signal(SIGSYS, manejador_SIGSYS);
        kill(getppid(), SIGALRM);
        printf("\nEL PROCESO MONITOR DEJA DOS SEGUNDOS DE ESPERA PARA VER EL ESTADO ACTUAL DE LAS APUESTAS\n");
        usleep(2000000);
        while(1){
            system("clear");
            imprimir_carrera(idCompartida, semid, nCaballos);
            /*REALIZAMOS UNA ESPERA DE 1 SEGUNDO PARA VISUALIZAR CORRECTAMENTE LA POSICION DE LOS CABALLOS*/
            usleep(2000000);
        }
        
        exit(EXIT_SUCCESS);
    } 
    return 0;
}