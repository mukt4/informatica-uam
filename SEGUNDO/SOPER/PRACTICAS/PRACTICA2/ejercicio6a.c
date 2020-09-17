#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <time.h>

#define NUM_PROC 5
#define SECS 3

int main (void){
    int pid, counter;
    sigset_t mascara;
    int m[4];
    
    sigemptyset(&mascara);
    
    pid = fork();
    if (pid == 0){
        alarm(SECS);
        while(1){
            sigemptyset(&mascara);
            m[0] = sigaddset(&mascara, SIGALRM); /*14 = señal SIGALRM*/
            m[1] = sigaddset(&mascara, SIGUSR1); /*16 = señal USR1*/
            m[2] = sigaddset(&mascara, SIGUSR2); /*17 = señal USR2*/
            
            if (m[0] == -1 || m[1] == -1 || m[2] == -1){
                printf("ERROR EN LA ADICION DE SEÑALES A LA MASCARA");
                exit(EXIT_FAILURE);
            }
            
            m[3] = sigprocmask(SIG_BLOCK, &mascara, NULL);
            
            if( m[3] == -1){
                printf("ERROR EN EL BLOQUEO DE LA MASCARA");
                exit(EXIT_FAILURE);
            }
            
            for (counter = 0; counter < NUM_PROC; counter++){
                
                printf("%d\n", counter);
                sleep(1);
            }
            
            m[0] = sigdelset(&mascara, SIGUSR2);
            
            if(m[0] == -1){
                printf("ERROR EN EL DELETEO DE LA SEÑAL SIGUR2");
                exit(EXIT_FAILURE);
            }
            
            m[3] = sigprocmask(SIG_UNBLOCK, &mascara, NULL);
            
            if(m[3] == -1){
                printf("ERROR EN EL DESBLOQUEO DE LA MASCARA");
                exit(EXIT_FAILURE);
            }
            
            sleep(3);
        }
    }
    
    while(wait(NULL)>0);
    exit(EXIT_SUCCESS);
}

/*Cuando reciba le señal el proceso terminara, el problema de este programa es que da la casualidad que en el segundo 40 del
bucle, es justo cuando se desbloquea la señal SIGALRM de la mascara, y se envía la señal. Si cambiamos el temporizador de la alarma
para que estos 2 sucesos no ocurran a la vez, el programa también terminará debido al wait(NULL)>0*/