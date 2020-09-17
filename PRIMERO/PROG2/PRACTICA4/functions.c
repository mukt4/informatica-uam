#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "functions.h"
#include "node.h"


/*
* En este fichero se definen las funciones de destrucción, copia e impresión de elementos a almacenar en
* una pila para distintos tipos de datos
*/

/* Las siguientes funciones se usarán cuando se quieran guardar enteros en la pila. Ojo! Estas funciones
reciben un puntero a entero, no una estructura con un puntero a entero (como en el ejercicio P2_E1) */
void destroy_intp_function(void* e){
    free((int*)e);
}

void * copy_intp_function(const void* e){
    int * dst;
    if (e == NULL)
        return NULL;
    dst = (int*)malloc(sizeof(int));
    /*Copiamos el elemento*/
    *(dst) = *((int*)e);
    return dst;
}

int print_intp_function(FILE * f, const void* e){
    if (f != NULL && e != NULL)
        return fprintf(f, "[%d]", *((int*)e));
    return -1;
}

int cmp_intp_function(const void* elem1, const void* elem2){
    if(!elem1 || !elem2){
        return 0;
    }
    if((*(int*)elem1)<(*(int*)elem2))
        return -1;
    else if((*(int*)elem1)>(*(int*)elem2))
        return 1;
    else
        return 0;
}

/* Las siguientes se usarán cuando se quieran guardar nodos en la pila */
void destroy_node_function(void* e){
    node_destroy((Node *)e);
}

void * copy_node_function(const void* e){
    return node_copy((Node *)e);
}

int print_node_function(FILE * f, const void* e){
    return node_print(f, (Node *)e);
}

int cmp_node_function(const void* elem1, const void* elem2){
    if(!elem1 || !elem2){
        return 0;
    }
    if(node_getId(elem1)<node_getId(elem2))
        return -1;
    else if(node_getId(elem1)>node_getId(elem2))
        return 1;
    else 
        return 0;
}

void destroy_char_function(void* e){
    if(!e)
        return;
    free(e);
}

void * copy_char_function(const void* e){
    void *aux=NULL;
    int length=0;
    if(!e)
        return NULL;
    length=strlen(e);
    aux = (char *) malloc(sizeof(char) * length + 1);
    strcpy(aux,e);
    return aux;
}

int print_char_function(FILE * f, const void* e){
    if (f != NULL && e != NULL)
        return fprintf(f, "%s ", (char*)e);
    return -1;
}

int cmp_char_function(const void* a, const void* b){
    int i=0;
    int length1=0;
    int length2=0;
    char *aux1=NULL;
    char *aux2=NULL;
    int max=0;
    if(!a || !b)
        return -1;
    aux1=(char*)a;
    aux2=(char*)b;
    if(strcmp(aux1,aux2)==0)
        return 0;
    length1=strlen(aux1);
    length2=strlen(aux2);
    if(length1<length2)
        max=length1;
    else
        max=length2;
    for(i=0;i<max;i++){
        if(aux1[i]<aux2[i])
            return -1;
        else if(aux1[i]>aux2[i])
            return 1;
    }
    if(length1>length2)
        return 1;
    else if(length1<length2)
        return -1;
    else
        return 0;
}