#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

/***
main: arma los manejadores de SIGTERM y SIGUSR1 y se pone a esperar
señales
***/
int main(int argc, char *argv[ ])
{
    void manejador_SIGUSR1 ( );
    void manejador_SIGTERM ( );
    signal(SIGTERM, manejador_SIGTERM); /* Armar la señal */
    signal(SIGUSR1, manejador_SIGUSR1); /*Armar la señal */
    while(1)
        pause(); /* Bloquea al proceso hasta que llegue una señal*/
}
/***
 manejador_SIGTERM saca un mensaje por pantalla y termina el proceso.
***/
void manejador_SIGTERM (int sig)
{
    printf("Terminación del proceso %d a petición del usuario \n",getpid( ));
    exit(-1);
}
/****
 Manejador_SIGUSR1: presenta un número aleatorio por pantalla.
***/
void manejador_SIGUSR1 (int sig)
{
    signal(sig, SIG_IGN);
    printf("%d \n", rand( ));
    signal(sig, manejador_SIGUSR1); /* Restaura rutina por defecto */
}