#include "alfabeto.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct _Alfabeto{
    char** simbolos;
    int tamanio;
    int last_elemento;
    ORDEN ordenado;
};

/*Funcion privada que utilizamos para swapear dos elementos de la lista*/
void swap(char** a, char** b);

Alfabeto* crearAlfabeto(int tamanio){
    Alfabeto* alfabeto = (Alfabeto*)malloc(sizeof(Alfabeto));
    
    /*Todos los punteros tienen el mismo tamanio*/
    alfabeto->simbolos = (char**)malloc(tamanio * sizeof(char*));
    alfabeto->ordenado = DESORDENADO;
    alfabeto->last_elemento = 0;
    alfabeto->tamanio = tamanio;
    
    return alfabeto;
}

void liberarAlfabeto(Alfabeto* alfabeto){
    int i;
    
    if(!alfabeto)
        return;
    
    for(i = 0; i < alfabeto->last_elemento; i++){
        free(alfabeto->simbolos[i]);
    }
    free(alfabeto->simbolos);
    free(alfabeto);
}

void ordenarAlfabeto(Alfabeto* alfabeto)
{
    int flag = 1;
    int i;
    int ip = 0;
    int iu = alfabeto->last_elemento;
    
    for (i = iu - 1; flag == 1 && i >= ip+1; i--){
        
        int j;
        
        flag = 0;
        
        for (j = ip; j < i; j++){
            if (strcmp(alfabeto->simbolos[j], alfabeto->simbolos[j+1]) > 0){
                swap(&alfabeto->simbolos[j], &alfabeto->simbolos[j+1]);
                flag = 1;
            }
        }
    }
    
    alfabeto->ordenado = ORDENADO;
}

void swap(char** a, char** b)
{
    char* aux = *b;
    *b = *a;
    *a = aux;
}

char* insertarElemento(Alfabeto* alfabeto, char* insertar){
    char* aux;
    
    if(!alfabeto || !insertar)
        return NULL;

    if(alfabeto->last_elemento == alfabeto->tamanio || buscarElemento(alfabeto, insertar) != -1) 
        return NULL;
        
    aux = (char*)malloc(strlen(insertar) * sizeof(char) + 1);
    strcpy(aux, insertar);
    alfabeto->simbolos[alfabeto->last_elemento] = aux;
    alfabeto->last_elemento++;
    alfabeto->ordenado = DESORDENADO;
    
    return aux;
}

int alfabeto_size(Alfabeto* alfabeto){
    if(!alfabeto)
        return -1;
    
    return alfabeto->last_elemento;
}

int buscarElemento(Alfabeto* alfabeto, char* elemento){
	int u = alfabeto->last_elemento - 1;
	int p = 0;
	int m;
	
	if(!alfabeto || !elemento)
	    return -1;
	   
	if(alfabeto->ordenado == DESORDENADO)
	    ordenarAlfabeto(alfabeto);
	
	while(p <= u){
	    
		m = ((u - p) / 2) + p;
		    
		if(strcmp(alfabeto->simbolos[m], elemento) == 0)
			return m;
			
		else if(strcmp(alfabeto->simbolos[m], elemento) < 0)
			p = m + 1;
			
		else
			u = m - 1;
	}
	
	return -1;
} 

void imprimirAlfabeto(Alfabeto* alfabeto){
    int i;
    
    if(!alfabeto)
        return;

    printf("[ ");
    for(i = 0; i < alfabeto->last_elemento - 1; i++){
        printf("%s",alfabeto->simbolos[i]);
        printf(", ");
    }
    printf("%s",alfabeto->simbolos[i]);
    printf(" ]\n");
}

ORDEN isOrdendado(Alfabeto* alfabeto){
    if(!alfabeto){
        return DESORDENADO;
    }
    return alfabeto->ordenado;
}

char* alfabeto_get_i(Alfabeto* alfabeto, int i){
    if(!alfabeto || alfabeto->last_elemento <= i)
        return NULL;
        
    return alfabeto->simbolos[i];
}

void imprimeAlfabeto(FILE* pf, Alfabeto* alfabeto){
    int i;
    
    if(!pf || !alfabeto)
        return;
    
    fprintf(pf,"A = { ");
    
    for(i = 0; i < alfabeto_size(alfabeto); i++){
        fprintf(pf, "%s ", alfabeto_get_i(alfabeto, i));
    }
    fprintf(pf, "}");
}

Alfabeto* alfabeto_getCopy(Alfabeto* alfabeto){
    Alfabeto* copia;
    int i;
    
    if(!alfabeto)
        return NULL;
    
    copia = crearAlfabeto(alfabeto->tamanio);
    
    for(i = 0; i < alfabeto->tamanio; i++){
        insertarElemento(copia, alfabeto_get_i(alfabeto, i));
    }
    
    return copia;
}
