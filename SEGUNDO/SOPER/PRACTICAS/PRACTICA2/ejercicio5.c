#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SECS 20

void captura(int sennal)
{
    printf("\nEstos son los numeros que me ha dado tiempo a contar en %d segundos\n", SECS);
    exit(0);
}

int main(int argc, char *argv [], char *env []){
    long int i;
    if (signal(SIGALRM, captura) == SIG_ERR)
    {
        puts("Error en la captura");
        exit (EXIT_FAILURE);
    }
    if (alarm(SECS))
        fprintf(stderr, "Existe una alarma previa establecida\n");
    for (i=0;;i++)
        fprintf(stdout, "%10ld\r", i);
    fprintf(stdout, "Fin del programa\n");
    exit(0);
}