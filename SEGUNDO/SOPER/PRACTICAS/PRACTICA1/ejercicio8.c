/*Variacion de combinatoria sin*/
/*paths /bin y /usr/bin du*/
/*SOMOS LA PAREJA 2 SKRRRR*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h> 

int main(int argc, char** argv){
    int pid = 0;
    int i;
    int status;
    char comando[500];
    char* prog[] = {NULL,NULL,NULL};
    char* path[] = {"/bin/", "/usr/bin/"};

    if(argc < 3){
        printf("\nERROR EN EL NUMERO DE ARGUMENTOS\n");
        exit(EXIT_FAILURE);
    }
    
    else{
        pid = fork();
        
        if(pid > 0){
            printf("\nSOY EL PADRE, MI ID ES %d Y LA DE MI HIJO ES %d\n", getpid(), pid);
        }else if(pid == 0){
            printf("\nSOY EL HIJO, MI ID ES %d Y LA DE MI PADRE ES %d\n", getpid(), getppid());
        }
        else{
            printf("\nERROR AL EMPLEAR FORK\n");
	        exit(EXIT_FAILURE);
        }
        
        if(pid == 0){
            for(i = 1; i < argc - 1; i++){
                pid = fork();
                
                if(pid > 0){
                    printf("\nSOY PADRE, MI ID ES %d Y LA DE MI HIJO ES %d\n", getpid(), pid);
                }else if(pid == 0){
                    printf("\nSOY NIETO, MI ID ES %d Y LA DE MI PADRE ES %d\n", getpid(), getppid());
                }else{
                    printf("\nERROR AL EMPLEAR FORK\n");
                    exit(EXIT_FAILURE);
                }
                
                if(strcmp(argv[i], "du") == 0){
                    strcpy(comando,path[1]);
                    strcat(comando,argv[i]);
                }
                
                else{
                    strcpy(comando,path[0]);
                    strcat(comando,argv[i]);
                }
                
                prog[0] = argv[i];
                prog[1] = argv[argc - 1];
                prog[2] = (char*)0;
                
                if(pid == 0){
                    if (strcmp(argv[argc - 1], "-l") == 0){
                        execl(comando, prog[0], NULL);
                        perror("Error en la ejecucion del comando");
                        exit(EXIT_FAILURE);
                    }
            
                    if (strcmp(argv[argc - 1], "-lp") == 0){
                        execlp(prog[0], prog[0], NULL);
                        perror("Error en la ejecucion del comando");
                        exit(EXIT_FAILURE);
                    }
                    
                    if (strcmp(argv[argc - 1], "-v") == 0){
                        execv(comando, prog);
                        perror("Error en la ejecucion del comando");
                        exit(EXIT_FAILURE);
                    }
                    
                    if (strcmp(argv[argc - 1], "-vp") == 0){
                        execvp(argv[i], prog);
                        perror("Error en la ejecucion del comando");
                        exit(EXIT_FAILURE);
                    }
                    
                    return 0;
                }
            }
        }
        
        wait(&status);
        exit(EXIT_SUCCESS);
    }
}