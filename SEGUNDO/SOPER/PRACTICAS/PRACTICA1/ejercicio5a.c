#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

#define NUM_PROC 6

int main (void)
{
	int pid;
	int status;
	int i;
	for (i=0; i <= NUM_PROC; i++){
		if (i % 2 != 0) {
			printf("FASE %d\n", i);
 			if ((pid=fork()) <0 ){
				printf("Error haciendo fork\n");
				exit(EXIT_FAILURE);
			}else if (pid ==0){
				printf("SOY HIJO MI PID ES %d Y EL DE MI PADRE ES %d\n", getpid(), getppid());
			}else{
				printf ("SOY PADRE MI PID ES %d Y EL DE MI HIJO ES %d\n", getpid(), pid);
				break;
			}
		}
	}
	wait(&status);
	exit(EXIT_SUCCESS);
}
