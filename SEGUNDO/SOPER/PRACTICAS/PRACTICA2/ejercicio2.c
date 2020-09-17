#define _BSD_SOURCE
#define _POSIX_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

#define NUM_HIJOS 4

int main (int argc, char *argv [], char *env []){
    int i, pid;
    
    for(i = 0; i < NUM_HIJOS; i++){
        pid = fork();
        if(pid < 0){
            perror("ERROR EN FORK");
            exit(EXIT_FAILURE);
        }else if(pid){
            /*EL PADRE DUERME 5 SEGUNDOS*/
            usleep(5000000);
            /*EL PADRE ENVIA SENIAL SIGTERM AL HIJO QUE ACABA DE CREAR*/
            kill(pid, SIGTERM);
        }else{
            printf("Soy el proceso hijo, PID: %d\n",getpid());
            /*EL HIJO DUERME 30 SEGUNDOS*/
            usleep(30000000);
            printf("Soy el proceso hijo, PID: %d y ya me toca terminar\n",getpid());
            exit(EXIT_SUCCESS);
        }
    }
    exit(EXIT_SUCCESS);
}