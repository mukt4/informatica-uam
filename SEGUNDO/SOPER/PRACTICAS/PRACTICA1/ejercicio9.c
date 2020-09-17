#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h> 

#define NUM_HIJOS 4
#define TAM 500

int main(){
	static int i;
	int j,op1,op2,status,pipe_status,pipe_status2,pid,fd[2],fdh[2],nrand;
	char pregunta[TAM],respuesta[TAM],aux;
	char* tok;
	double fact;
	

	printf("Introduce el operando 1: ");
	scanf("%d",&op1);
	printf("Introduce el operando 2: ");
	scanf("%d",&op2);
	
	sprintf(pregunta, "%d,%d",op1,op2);
	
	for(i = 0; i < NUM_HIJOS; i++){
		/*CANAL DE ESCRITURA PADRE*/
		pipe_status = pipe(fd);
		/*CANAL DE ESCRITURA HIJO*/
		pipe_status2 = pipe(fdh);

		if(pipe_status == -1){
			perror("Error creando la tuberia\n");
			exit(EXIT_FAILURE);
		}
		if(pipe_status2 == -1){
			perror("Error creando la tuberia\n");
			exit(EXIT_FAILURE);
		}
		pid = fork();
		if(pid == -1){
			perror("Error en fork\n");
			exit(EXIT_FAILURE);
		}
		if(pid > 0){
			/*PREPARACION PIPES PADRE*/
			close(fd[0]);
			close(fdh[1]);
			/*ESCRITURA PIPE FD*/
			write(fd[1],pregunta,strlen(pregunta));
			/*ESPERA A QUE EL HIJO ACABE*/
			wait(&status);
			/*APERTURA DE LECTURA FDH*/
			bzero(respuesta,TAM);
			read(fdh[0],respuesta,sizeof(respuesta));
			printf("PROCESO PADRE OBTIENE RESPUESTA DE HIJO CON PID %d / %s\n",pid,respuesta);
		}
		/*LOS HIJOS CIERRAN EL CANAL DE ESCRITURA Y EL PADRE EL DE LECTURA*/
		else{
			/*PREPARACION PIPES*/
			close(fd[1]);
			close(fdh[0]);
			/*LECTURA PIPE FD*/
			read(fd[0],pregunta,sizeof(pregunta));
			tok = strtok(pregunta,",");
			op1=atoi(tok);
			tok = strtok(NULL,",");
			op2=atoi(tok);
			if(i == 0){
				fact = 1;
				for(j = 0; j < op2; j++){
					fact = fact * op1;
				}
				bzero(respuesta,TAM);
				sprintf(respuesta,"Datos enviados a traves de la tuberia por el proceso PID = %d. Operando 1: %d. Operando 2: %d. Potencia: %.2lf\n",getpid(),op1,op2,fact);
			}
			else if(i == 1){
				fact = 1;
				for(j = 1; j <= op1; j++){
					fact = j * fact;
				}
				fact = fact / (op2 * 1.0);
				bzero(respuesta,TAM);
				sprintf(respuesta,"Datos enviados a traves de la tuberia por el proceso PID = %d. Operando 1: %d. Operando 2: %d. Factorial op1 / op2: %.3lf\n",getpid(),op1,op2,fact);
			}
			else if(i == 2){
				for(j = 0; j<strlen(tok) ; j++){
					nrand = rand() % strlen(tok);
					aux = tok[nrand];
					tok[nrand] = tok[j];
					tok[j] = aux;
				}
				bzero(respuesta,TAM);
				sprintf(respuesta,"Datos enviados a traves de la tuberia por el proceso PID = %d. Operando 1: %d. Operando 2: %d. Permutacion op1 - op2: %s\n",getpid(),op1,op2,tok);
			}else{
				bzero(respuesta,TAM);
				sprintf(respuesta,"Datos enviados a traves de la tuberia por el proceso PID = %d. Operando 1: %d. Operando 2: %d. Suma: %d\n",getpid(),op1,op2,op1 + op2);
			}
			/*ESCRITURA PIPE FDH*/
			write(fdh[1], respuesta, strlen(respuesta));
			exit(EXIT_SUCCESS);
		}
	}
	exit(EXIT_SUCCESS);
}

		

		
	
