#include "palabra.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>

#define TAM 5

int test_crearPalabra(){
    Palabra* palabra;
    
    palabra = crearPalabra();
    
    if(!palabra)
        return 0;
    
    return 1;
}

int test_liberarPalabra(){
    Palabra* palabra;
    
    palabra = crearPalabra();
    
    liberarPalabra(palabra);
    
    return 1;
}

int test_insertarLetraPalabra1(){
    Palabra* palabra;

    palabra = crearPalabra();
    
    insertarLetraPalabra(palabra, "a");
    
    if(getCountSimbolosPalabraAct(palabra) != 1)
        return 0;
        
    liberarPalabra(palabra);
    
    return 1;
}

int test_insertarLetraPalabra2(){
    Palabra* palabra;
    char** aux;
    int i;
    char insertar[TAM][TAM] = {"A", "B", "C", "D", "E"};
    
    palabra = crearPalabra();
    
    for(i = 0; i < TAM; i++)
        insertarLetraPalabra(palabra, insertar[i]);
        
    if(getCountSimbolosPalabraAct(palabra) != TAM)
        return 0;
        
    aux = getSimbolosPalabraAct(palabra);
    
    for(i = 0; i < TAM; i++){
        if(strcmp(insertar[i], aux[i]) != 0)
            return 0;
    }
    liberarPalabra(palabra);
    
    return 1;
}

int test_procesarSimbolo(){
    Palabra* palabra;
    int i;
    char insertar[TAM][TAM] = {"A", "B", "C", "D", "E"};
    
    palabra = crearPalabra();
    
    for (i = 0; i < TAM; i++)
        insertarLetraPalabra(palabra, insertar[i]);

    procesarSimbolo(palabra);

    if (getCountSimbolosPalabraAct(palabra) != TAM)
        return 0;
    
    if (strcmp(getSimboloActual(palabra), insertar[1]) != 0)
        return 0;
    
    liberarPalabra(palabra);
    
    return 1;
}

int main(){
    assert(test_crearPalabra());
    printf("Test crear palabra...[OK]\n");
    assert(test_liberarPalabra());
    printf("Test liberar palabra...[OK]\n");
    assert(test_insertarLetraPalabra1());
    assert(test_insertarLetraPalabra2());
    printf("Test insertar palabra...[OK]\n");
    assert(test_procesarSimbolo());
    printf("Test procesar simbolo de palabra...[OK]\n");

    printf("\nTODOS LOS TEST DE PALABRA PASADOS CORRECTAMENTE.\n");
    
    return 0;
}