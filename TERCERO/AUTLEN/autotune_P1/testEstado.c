#include "estado.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define TAM 20

int test_crearEstado(){
    Estado* estado;
    char nombre[TAM] = "Estado1";
    
    estado = crearEstado(nombre, INICIAL);
    if(estado ==  NULL)
        return 0;
    
    if(strcmp(estado_getNombre(estado), nombre) != 0)
        return 0;
    
    destruirEstado(estado);

    return 1;
}

int test_imprimeEstado(){
    Estado* estado1;
    Estado* estado2;
    Estado* estado3;
    char nombre1[TAM] = "Estado1", nombre2[TAM] = "Estado2", nombre3[TAM] = "Estado3";
    
    estado1 = crearEstado(nombre1, INICIAL);
    estado2 = crearEstado(nombre2, NORMAL);
    estado3 = crearEstado(nombre3, FINAL);
    printf("\n");
    imprimeEstado(stdout, estado1);
    printf("\n");
    imprimeEstado(stdout, estado2);
    printf("\n");
    imprimeEstado(stdout, estado3);
    
    destruirEstado(estado1);
    destruirEstado(estado2);
    destruirEstado(estado3);
    
    return 1;
}

int main(){
    assert(test_crearEstado());
    printf("Test crear estado...[OK]\n");
    assert(test_imprimeEstado());
    printf("\nTests imprimir estado...[OK]\n\n");
    
    printf("\nTODOS LOS TEST DE ESTADO PASADOS CORRECTAMENTE.\n");
    
    return 0;
}