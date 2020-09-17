#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

#include "aleatorio.h"

int contador = 0;

int tirada_normal(){
    srand(getpid());
    
    return (rand() % 6) + 1;
}

int tirada_ganadora(){
    srand(getpid());
    
    return (rand() % 7) + 1;
}

int tirada_remontadora(){
    int suma;
    
    suma = tirada_normal();
    suma += tirada_normal();
    
    return suma;
}

int caballo_aleatorio(int nCaballos){
    if(contador == 0){
        srand(getpid());
    }
    contador++;
        
    return (rand() % nCaballos);
}

double dinero_aleatorio(double dinero){
    srand48(getpid());
    
    return drand48() * dinero;
}