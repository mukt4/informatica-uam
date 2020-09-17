#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

#define TAM 80

typedef struct{
	int* entero;
	char* cadena;
}estructura;

int main (void){
	estructura* reserva;
	int pid;
	int status;	
	
	reserva = (estructura*)malloc(sizeof(estructura));
	reserva->cadena = (char*)malloc(TAM * sizeof(char));
	reserva->entero = (int*)malloc(sizeof(int));

	pid = fork();

	if(pid < 0){
		printf("Error haciendo fork\n");
		exit(EXIT_FAILURE);
	}
	else if(pid == 0){
		printf("SOY EL HIJO MI PID ES %d Y EL DE MI PADRE ES %d\n",getpid(), getppid());
		fgets(reserva->cadena,TAM,stdin);
		free(reserva->cadena);
		free(reserva->entero);
		free(reserva);
	}
	else{
		printf("SOY EL PADRE MI PID ES %d Y EL DE MI HIJO ES %d\n",getpid(), pid);
		wait(&status);
		if(reserva->cadena == NULL){
			printf("EL PADRE NO PUEDE ACCEDER A LA CADENA CREADA POR EL HIJO\n");
			exit(EXIT_SUCCESS);
		}
		free(reserva->cadena);
		free(reserva->entero);
		free(reserva);
	}
	exit(EXIT_SUCCESS);
		
}
