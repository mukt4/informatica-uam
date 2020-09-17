#include <stdio.h>

#include "aleatorio.h"

int tirada_normal(){
    srand(time(NULL));
    
    return (rand() % 6) + 1;
}

int tirada_ganadora(){
    srand(time(NULL));
    
    return (rand() % 7) + 1;
}

int tirada_remontadora(){
    int suma;
    
    suma = tirada_normal();
    suma += tirada_normal();
    
    return suma;
}

int caballo_aleatorio(int nCaballos){
    srand(time(NULL));
    
    return (rand() % nCaballos) + 1;
}

double dinero_aleatorio(double dinero){
    srand48(time(NULL));
    
    return drand48() * dinero;
}