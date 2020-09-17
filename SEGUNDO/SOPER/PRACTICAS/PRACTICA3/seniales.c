#include <signal.h>

#include "seniales.h"

void manejador_SIGINT(int sig){
    printf("Se ha recibido la senial CTRL + C de finalizacion de programa");
    /*MATAR A TODOS LOS PROCESOS HIJOS Y AVISAR AL MONITOR PARA QUE REALICE SUS MOVIDAS*/
    exit(EXIT_SUCCESS);
}

void manejador_SIGUSR1(int sig){
    printf("Se ha recibido la senial de final de tiempo de apuestas");
}

void manejador_SIGUSR2(int sig){
    printf("Se han generado %d caballos", sig);
}

void manejador_SIGALALRM(int sig){
    printf("El proceso principal envia la senial y esta a la espera de que los caballos realicen su tirada");
}

void manejador_SIGTERM(int sig){
    printf("La carrera ha terminado");
}

