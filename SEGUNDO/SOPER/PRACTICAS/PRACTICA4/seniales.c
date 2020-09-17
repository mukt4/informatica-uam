#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#include "seniales.h"

void manejador_SIGINT(int sig){
    printf("\nEL usuario ha enviado la senial de fin del programa CTRL + C\n");
    exit(EXIT_SUCCESS);
}

void manejador_SIGALRM(int sig){
    printf("\nSe ha terminado la cuenta atras, comienza la carrera\n");
}

void manejador_SIGUSR1(int sig){
    exit(EXIT_SUCCESS);
}

void manejador_SIGUSR2(int sig){
    printf("\nSoy un apostante y el proceso principal me ha avisado de que se ha acdabado el tiempo de apuestas\n");
    exit(EXIT_SUCCESS);
}

void manejador_SIGTERM(int sig){
    exit(EXIT_SUCCESS);
}

void manejador_SIGTRAP(int sig){
    printf("\nSoy un hilo y el proceso gestor de apuestas me ha avisado de que se ha acabado mi trabajo\n");
    exit(EXIT_SUCCESS);
}

void manejador_SIGSYS(int sig){
    printf("\nSoy el proceso monitor y el proceso principal me ha avisado que ha acabado la carrera\n");
    exit(EXIT_SUCCESS);
}
