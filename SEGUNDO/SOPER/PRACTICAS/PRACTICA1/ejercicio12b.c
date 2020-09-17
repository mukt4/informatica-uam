#define _BSD_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <sys/types.h> 
#include <pthread.h>

#define TAM 100

void* primos (void* arg);

typedef struct{
	int* entero;
	char* cadena;
} estructura;

int main(int argc, char** argv){
    estructura* reserva = NULL;
    int i = 0;
    pthread_t h1[TAM];
    
    double secs = 0;
    clock_t t_ini, t_fin;
    
    if(argc < 2){
        printf("\nERROR EN EL NUMERO DE ARGUMENTOS\n");
        exit(EXIT_FAILURE);
    }
    
    reserva = (estructura*)malloc(sizeof(estructura));
	reserva->cadena = (char*)malloc(TAM * sizeof(char));
	reserva->entero = (int*)malloc(sizeof(int));
    
    t_ini = clock();
    for(i = 0; i < TAM; i++){
        pthread_create(&h1[i], NULL, primos, (void*)argv[1]);
    }
    
    t_fin = clock();
    for(i = 0; i < TAM; i++){
        pthread_join(h1[i], NULL);
    }
    
    secs = (double)(t_fin - t_ini) / CLOCKS_PER_SEC;
    
    printf("Se han tardado: %f secs en hacer la creación de hilos, ejecución y finalización\n", secs);
    
    free(reserva->cadena);
	free(reserva->entero);
	free(reserva);
    
    exit(EXIT_SUCCESS);
}

void* primos (void* arg) {
    int p_c = 0, dv = 2, z = 0;
    int j;
    int numero_primos;

    numero_primos = atoi( (char* )arg);
    
    while (p_c < numero_primos){
        for(j = 1, z = 0; j <= dv; j++){
            if(dv % j == 0){
                z++;
            }
        }
                    
        if (z == 2){
            p_c++;
        }
                    
        dv++;
    }
    
    pthread_exit(NULL);
    
}
