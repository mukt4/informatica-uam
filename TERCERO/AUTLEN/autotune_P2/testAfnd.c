#include "afnd.h"

#include <stdio.h>
#include <assert.h>

int test_crearAfnd(){
    AFND* afnd;
    
    afnd = AFNDNuevo("afnd", 3, 3);
    
    if(afnd == NULL)
        return 0;
    
    AFNDElimina(afnd);

    return 1;
}

int test_destruirAFND(){
    AFND* afnd = NULL;
    
    afnd = AFNDNuevo("afnd", 3, 3);
    
    AFNDElimina(afnd);
    
    return 1;
}

int test_insertaSimbolo(){
    AFND* afnd;
    
    afnd = AFNDNuevo("afnd", 3, 3);
    
    AFNDInsertaSimbolo(afnd,"0");
    
    if (getCountSimbolosAFND(afnd) == 1){
        AFNDElimina(afnd);
        return 1;
    }
    else {
        AFNDElimina(afnd);
        return 0;
    }
}

int test_insertaEstado(){
    AFND* afnd;
    
    afnd = AFNDNuevo("afnd", 3, 3);
    
    AFNDInsertaEstado(afnd,"q0",INICIAL);
    
    if (get_indiceEstado(afnd, "q0") == 0){
        AFNDElimina(afnd);
        return 1;
    }
    else {
        AFNDElimina(afnd);
        return 0;
    }
}

int main(){
    assert(test_crearAfnd());
    printf("Test crear afnd...[OK]\n");
    assert(test_destruirAFND());
    printf("Test destruir afnd...[OK]\n");
    assert(test_insertaSimbolo());
    printf("Test insertar simbolo afnd...[OK]\n");
    assert(test_insertaEstado());
    printf("Test insertar estado afnd...[OK]\n");

    printf("\nTODOS LOS TEST DE AFND PASADOS CORRECTAMENTE.\n");
    
    return 0;
}