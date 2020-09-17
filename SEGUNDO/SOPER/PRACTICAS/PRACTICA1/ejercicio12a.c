#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>
#include <sys/types.h> 

#define TAM 100

typedef struct{
	int* entero;
	char* cadena;
}estructura;

int main(int argc, char** argv){
    estructura* reserva = NULL;
    int n = 0;
    int pid, status;
    
    int i , j;
    int dv = 2; /*Consideraremos que el 1 no es primo*/
    int p_c = 0, z;
    
    double secs = 0;
    clock_t t_ini, t_fin;
    
    if(argc < 2){
        printf("\nERROR EN EL NUMERO DE ARGUMENTOS\n");
        exit(EXIT_FAILURE);
    }
    
    reserva = (estructura*)malloc(sizeof(estructura));
	reserva->cadena = (char*)malloc(TAM * sizeof(char));
	reserva->entero = (int*)malloc(sizeof(int));
	
    n = atoi(argv[1]);
    
    t_ini = clock();
    
    pid = fork();
    if(pid > 0){
        for(i = 0; i <= 98; i++){
            pid = fork();
            if(pid == 0) {
                i = 100;
                
                while (p_c < n){
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
            
                free(reserva->cadena);
        		free(reserva->entero);
        		free(reserva);
        		
                exit(EXIT_SUCCESS);
            }
        }
    }
    
    dv = 2; 
    
    if(pid == 0){
        while (p_c < n){
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
        
        free(reserva->cadena);
        free(reserva->entero);
        free(reserva);
        		
        exit(EXIT_SUCCESS);
    }
    
    for(i = 0; i < 99; i++){
        wait(&status);
    }
    
    t_fin = clock();
    secs = (double)(t_fin - t_ini) / CLOCKS_PER_SEC;
    
    printf("Se han tardado: %f secs en hacer la creación de procesos, ejecución y finalización\n", secs);
    
    free(reserva->cadena);
	free(reserva->entero);
	free(reserva);
    
    exit(EXIT_SUCCESS);
    
}
