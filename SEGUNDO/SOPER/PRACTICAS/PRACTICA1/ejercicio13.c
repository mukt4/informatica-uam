#define _BSD_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

int a1,a2;

typedef struct{
	int **fila;
	int dimension;
	int multiplicador;
}matriz;

void* producto1(void* matriz1){
	int j;
	
	for(a1 = 0; a1 < (((matriz*)matriz1)->dimension); a1++){
		printf("\nHilo 1 multiplicando fila %d resultado ",a1);
		for(j = 0; j < (((matriz*)matriz1)->dimension); j++){
			printf("%d ",(((matriz*)matriz1)->fila[a1][j]) * (((matriz*)matriz1)->multiplicador));
		}
		printf(" - el Hilo 2 va por la fila %d",a2);
		fflush(stdout);
	}
	pthread_exit(NULL);
}

void* producto2(void* matriz2){
	int j;
	
	for(a2 = 0; a2 < (((matriz*)matriz2)->dimension); a2++){
		printf("\nHilo 2 multiplicando fila %d resultado ",a2);
		for(j = 0; j < (((matriz*)matriz2)->dimension); j++){
			printf("%d ",(((matriz*)matriz2)->fila[a2][j]) * (((matriz*)matriz2)->multiplicador));
		}
		printf(" - el Hilo 1 va por la fila %d",a1);
		fflush(stdout);
	}
	pthread_exit(NULL);
}

int main(int argc, char** argv){
	pthread_t h1;
	pthread_t h2;
	int dimension,mul1,mul2,i,j;
	matriz* matriz1;
	matriz* matriz2;
	
	matriz1 = (matriz*)malloc(sizeof(matriz));
	matriz2 = (matriz*)malloc(sizeof(matriz));

	printf("Introduzca dimension de la matriz cuadrada: \n");
	scanf("%d",&dimension);
	printf("\nIntroduzca multiplicador 1: \n");
	scanf("%d",&mul1);
	printf("\nIntroduzca multiplicador 2: \n");
	scanf("%d",&mul2);
	
	matriz1->dimension = dimension;
	matriz2->dimension = dimension;
	matriz1->multiplicador = mul1;
	matriz2->multiplicador = mul2;
	
	matriz1->fila = (int**)malloc(dimension*sizeof(int*));
	for(i = 0; i < dimension; i++){
		(matriz1)->fila[i] = (int*)malloc(dimension*sizeof(int));
	}
	matriz2->fila = (int**)malloc(dimension*sizeof(int*));
	for(i = 0; i < dimension; i++){
		(matriz2)->fila[i] = (int*)malloc(dimension*sizeof(int));
	}
	printf("Introduzca la matriz 1: \n");
	for(i = 0; i < dimension; i++){
		for(j = 0; j < dimension; j++){
			setbuf(stdin,NULL);
			scanf("%d",&((matriz1)->fila[i][j]));
		}
	}
	printf("Introduzca la matriz 2: \n");
	for(i = 0; i < dimension; i++){
		for(j = 0; j < dimension; j++){
			setbuf(stdin,NULL);
			scanf("%d",&((matriz2)->fila[i][j]));
		}
	}

	pthread_create(&h1, NULL , producto1 , (void*)matriz1);
	pthread_create(&h2, NULL , producto2 , (void*)matriz2);
	
	pthread_join(h1,NULL);
	pthread_join(h2,NULL);

	for(i = 0; i < dimension; i++){
		free(matriz1->fila[i]);
	}
	free(matriz1->fila);

	for(i = 0; i < dimension; i++){
		free(matriz2->fila[i]);
	}
	free(matriz2->fila);
	
	free(matriz1);
	free(matriz2);

	printf("\nFIN DEL PROGRAMA\n");
	
	exit(EXIT_SUCCESS);
}
	
