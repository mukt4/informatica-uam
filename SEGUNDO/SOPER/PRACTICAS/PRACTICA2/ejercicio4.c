#define _BSD_SOURCE
#define _POSIX_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

void manejador_SIGUSR1(int sig);
void manejador_SIGUSR2(int sig);

int main(int argc, char *argv [], char *env []){
    int i, pid, j;
    static int last_pid = -1;
    
    if(argc < 2){
        printf("\nERROR EN EL NUMERO DE ARGUMENTOS\n");
        exit(EXIT_FAILURE);
    }
    
    for( i = 0; i < atoi(argv[1]); i++){
        pid = fork();
        if(pid < 0){
            perror("\nERROR EN EL FORK\n");
            exit(EXIT_FAILURE);
        }
        else if(pid){
                signal(SIGUSR1, manejador_SIGUSR1);
                pause();
                last_pid = pid;
        }
        else{
            if(last_pid != -1){
                kill(last_pid, SIGUSR2);
                printf("SOY EL HIO %d Y VOY A MANDAR LA SENIAL AL HIJO %d DE QUE PUEDE TERMINAR",getpid(), last_pid);
            }
            for(j = 0; j < 10; j++){
                printf("\nSoy %d y estoy working\n", getpid());
                usleep(1000000);
            }
            kill(getppid(), SIGUSR1);
            signal(SIGUSR2, manejador_SIGUSR2);
            while(1){
                printf("\nSoy %d y estoy working\n", getpid());
                usleep(1000000);
            }
        }
    }
    printf("\nESTOY HARTO DE TENER HIJOS, VOY A MANDAR UNA SENIAL A MI ULTIMO HIJO Y VOY PASAR A VIVIR UNA VIDA DE LUJURIA Y EXCESOS\n");
    kill(last_pid, SIGUSR2);
    exit(EXIT_SUCCESS);
}

void manejador_SIGUSR1 (int sig){
    printf("\nSOY EL PADRE HE RECIBIDO LA SENIAL, MI HIJO ESTA CANSADO ASI QUE VOY A TENER OTRO\n");
}

void manejador_SIGUSR2(int sig){
    printf("\nSOY EL HIJO Y ME ACABAN DE AVISAR DE QUE PUEDO TOMARME UN DESCANSO, HARTO DE ESTA VIDA DE DURO TRABAJO, SUICIDARME ES LA MEJOR OPCION\n");
    exit(EXIT_SUCCESS);
}