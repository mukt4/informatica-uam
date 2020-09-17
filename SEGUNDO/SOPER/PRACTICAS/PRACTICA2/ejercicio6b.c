#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <time.h>

#define NUM_PROC 5
#define ALARM 40

void manejador_SIGTERM (int sig);

int main (void){
    int pid, counter/*, c*/;
    pid = fork();
    sigset_t mascara;
    sigaddset(&mascara, SIGALRM);
    
    if(pid < 0){
        perror("\nERROR EN EL FORK\n");
        exit(EXIT_FAILURE);
    }
    
    if (pid == 0){
        while(1){
            for (counter = 0; counter < NUM_PROC; counter++){
                printf("%d\n", counter);
                sleep(1);
            }
            
            sleep(3);
        }
        sigsuspend(&mascara);
        kill(pid, SIGTERM);
    }
    
    else{
        alarm(ALARM);
        signal(SIGTERM, manejador_SIGTERM);
    }
    
    while(wait(NULL)>0);
    exit(EXIT_SUCCESS);
}

void manejador_SIGTERM (int sig){
    printf("\n “Soy %d y he recibido la señal SIGTERM”\n", getpid());
    exit(EXIT_SUCCESS);
}