#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
void captura (int sennal){
    printf ("Capturada la señal %d  \n", sennal);
    fflush (NULL);
    return;
}

int main (int argc, char *argv [], char *env []){
    if (signal (SIGINT, captura) == SIG_ERR){
        puts ("Error en la captura");
        exit (1);
    }
    while (1);
    exit (0);
}