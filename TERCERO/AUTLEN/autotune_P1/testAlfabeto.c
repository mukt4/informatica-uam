#include "alfabeto.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define TAM 20

int test_crearAlfabetoChar(){
    Alfabeto* alfabeto;
    
    alfabeto = crearAlfabeto(TAM);
    
    if(!alfabeto)
        return 0;
    
    liberarAlfabeto(alfabeto);
    return 1;
}

int test_destruirAlfabetoChar1(){
    Alfabeto* alfabeto;
    
    alfabeto = crearAlfabeto(TAM);

    liberarAlfabeto(alfabeto);
    
    return 1;
}

int test_destruirAlfabetoChar2(){
    Alfabeto* alfabeto;
    char test[TAM] = "Test";
    
    alfabeto = crearAlfabeto(TAM);
    insertarElemento(alfabeto, test);
    liberarAlfabeto(alfabeto);
    
    if(test == NULL){
        return 0;
    }
    
    return 1;
}

int test_insertarAlfabetoChar(){
    Alfabeto* alfabeto;
    char cadenasInsertar[TAM][TAM] = {"Manue", "ErSergio", "abracadabra", "tolay", "pimpin", "test", "wut","eing","cadenaaaaaaaaas", "cbo", "loquete", "andrew", "biblioteca", "capoots", "otra", "yotra", "kike", "autlen", "paco","abanico"};
    int i;
    char* aux;
    
    alfabeto = crearAlfabeto(TAM);
    
    for(i = 0; i < TAM ; i++){
        aux = insertarElemento(alfabeto, cadenasInsertar[i]);
        if(!aux){
            liberarAlfabeto(alfabeto);
            return 0;
        }
    }
    
    liberarAlfabeto(alfabeto);
    return 1;
}

int test_ordernarAlfabetoChar(){
    char cadenasInsertar[TAM][TAM] = {"c", "a", "z", "l", "g", "h", "j", "m", "x", "q", "w", "f", "s", "p", "o", "v", "r", "y", "u", "i"};
    char cadenasOrdenadas[TAM][TAM] = {"a", "c", "f", "g", "h", "i", "j", "l", "m", "o", "p", "q", "r", "s", "u", "v", "w", "x", "y", "z"};
    Alfabeto* alfabeto;
    int i;
    
    alfabeto = crearAlfabeto(TAM);
    
    for(i = 0; i < TAM; i++){
        insertarElemento(alfabeto, cadenasInsertar[i]);
    }
    ordenarAlfabeto(alfabeto);
    
    for(i = 0; i < TAM; i++){
        if(strcmp(cadenasOrdenadas[i], alfabeto_get_i(alfabeto, i)) != 0){
            liberarAlfabeto(alfabeto);
            return 0;
        }
    }
    
    liberarAlfabeto(alfabeto);
    return 1;
}

int test_busquedaAlfabetoChar1(){
    char cadenasInsertar[TAM][TAM] = {"c", "a", "z", "l", "g", "h", "j", "m", "x", "q", "w", "f", "s", "p", "o", "v", "r", "y", "u", "i"};
    Alfabeto* alfabeto;
    int aux;
    int i;
    
    alfabeto = crearAlfabeto(TAM);
    
    for(i = 0; i < TAM; i++){
        insertarElemento(alfabeto, cadenasInsertar[i]);
    }
    ordenarAlfabeto(alfabeto);
    
    aux = buscarElemento(alfabeto, "q");
    
    if(aux == -1){
        liberarAlfabeto(alfabeto);
        return 0;
    }
    
    if(aux == 11){
        return 1;
    }
    
    return 0;
}

int test_busquedaAlfabetoChar2(){
    char cadenasInsertar[TAM][TAM] = {"c", "a", "z", "l", "g", "h", "j", "m", "x", "q", "w", "f", "s", "p", "o", "v", "r", "y", "u", "i"};
    Alfabeto* alfabeto;
    int aux;
    int i;
    
    alfabeto = crearAlfabeto(TAM);
    
    for(i = 0; i < TAM; i++){
        insertarElemento(alfabeto, cadenasInsertar[i]);
    }
    ordenarAlfabeto(alfabeto);
    
    aux = buscarElemento(alfabeto, "b");
    
    if(aux == -1){
        liberarAlfabeto(alfabeto);
        return 1;
    }
        
    liberarAlfabeto(alfabeto);
    return 0;
}

int test_busquedaAlfabetoChar3(){
    char cadenasInsertar[TAM][TAM] = {"c", "a", "z", "l", "g", "h", "j", "m", "x", "q", "w", "f", "s", "p", "o", "v", "r", "y", "u", "i"};
    Alfabeto* alfabeto;
    int aux;
    int i;
    
    alfabeto = crearAlfabeto(TAM);
    
    for(i = 0; i < TAM; i++){
        insertarElemento(alfabeto, cadenasInsertar[i]);
    }
    
    aux = buscarElemento(alfabeto, "a");
    
    if(aux == -1){
        liberarAlfabeto(alfabeto);
        return 0;
    }
    
    liberarAlfabeto(alfabeto);  
    return 1;
}



int main(){
    assert(test_crearAlfabetoChar());
    printf("Test crear alfabeto...[OK]\n");
    assert(test_insertarAlfabetoChar());
    printf("Test insertar alfabeto...[OK]\n");
    assert(test_destruirAlfabetoChar1());
    assert(test_destruirAlfabetoChar2());
    printf("Tests destruir alfabeto...[OK]\n");
    assert(test_ordernarAlfabetoChar());
    printf("Test ordenar alfabeto...[OK]\n");
    assert(test_busquedaAlfabetoChar1());
    assert(test_busquedaAlfabetoChar2());
    assert(test_busquedaAlfabetoChar3());
    printf("Tests busqueda alfabeto...[OK]\n");
    
    printf("\nTODOS LOS TEST DE ALFABETO PASADOS CORRECTAMENTE.\n");
    return 0;
}