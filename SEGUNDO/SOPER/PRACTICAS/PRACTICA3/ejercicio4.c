#define _SVID_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <string.h>
#include <errno.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <time.h>
#include <sys/wait.h>
#include <signal.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#define FILEKEY "/bin/cat"

#define KEY 4558

int nAleatorio();
int numAleatorio();

void* impresion_numeros(void* descriptor);
void* map_size_coma(void* descriptor);


/*Global variables*/

int main(int argc, char *argv[]){
    int descriptor; /*Variable to identify the file*/
    int err = -2; /*Just for checking threads errors*/
    
    descriptor = open("fichero.txt", O_CREAT | O_RDWR); /*creating file "fichero.txt" with reading and writing permission*/
    
    if(descriptor == -1){
        close(descriptor);
        perror("\nFallo al crear el fichero\n");
        exit(EXIT_FAILURE);
    }
    
    pthread_t h1; /*Strings of the process*/
    pthread_t h2; /*Strings of the process*/
    
    /*Creating and executing the first thread*/
    err = pthread_create(&h1, NULL, impresion_numeros, (void*) &descriptor);
    if(err != 0){
        close(descriptor);
        perror("\nFallo del primer hilo\n");
        exit(EXIT_FAILURE);
    }
    
    /*waiting the first thread*/
    pthread_join(h1,NULL); 
    printf("\nPrimer hilo acabado");
    
    /*Creating and executing the second thread*/
    err = pthread_create(&h2, NULL, map_size_coma, (void*) &descriptor);
    if(err != 0){
        close(descriptor);
        perror("\nFallo del segundo hilo\n");
        exit(EXIT_FAILURE);
    }
    
    /*waiting the second thread*/
    pthread_join(h2,NULL); 
    printf("\nSegundo hilo acabado");

    /*Closing the file*/
    if(close(descriptor) == -1){
        perror("\nFallo al cerrar el descriptor\n");
        exit(EXIT_FAILURE);
    }
    
    return 0;
}

void* impresion_numeros(void* descriptor){
    int descrip = (*(int*)descriptor);
    int i = 0;
    int n = nAleatorio();
    int aleat = 0;
    char buffer[n]; /*variable declared to transform the int aleat to a char and write it down in the file*/
    int err = -1;
    
    printf("\n primer hilo");
    
    if(n < 1000 || n > 2000){
        close(descrip);
        perror("\nFallo en el rango de generación de n\n");
        exit(EXIT_FAILURE);
    }
    
    for(i = 0; i < n; i++){
        aleat = numAleatorio();

        if(aleat < 100 || aleat > 1000){
            close(descrip);
            perror("\nFallo en el rango de generación de aleat\n");
            exit(EXIT_FAILURE);
        }
        
        sprintf(buffer, "%d,", aleat);
        
        /*Escritura en el fichero/descriptor*/
        err += write(descrip, &buffer, strlen(buffer)); 
        
        if(err == -1){
            close(descrip);
            perror("\nFallo en el proceso de escritura del hilo\n");
            exit(EXIT_FAILURE);
        }
    }
    
    pthread_exit(NULL);
}

void* map_size_coma(void* descriptor){
    int descrip = (*(int*)descriptor);
    int z = 0; /*Variable for loops*/
    struct stat stat; /*Struct to get the size of the file*/
    char *map; /*Variable that will be the returned value of the function mmap*/
    
    printf("Inicio segundo hilo");
    
    if(fstat(descrip, &stat) == -1){
        close(descrip);
        perror("\nFallo al obtener la longitud en bytes del fichero, hilo2\n");
        exit(EXIT_FAILURE);
    }
    
    /*Maping file*/
    map = mmap(0, stat.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, descrip, 0); 
    if(map == MAP_FAILED){
        close(descrip);
        perror("\nFallo al mapear el fichero\n");
        exit(EXIT_FAILURE);
    } 
    
    /*Reading and writing in the file*/
    for(z = 0; z < stat.st_size; z++){
        if(map[z] == ','){
            map[z] = ' '; 
        }
        
        printf("%c", map[z]);
    }
    
    /*We free the map mmaped memory*/
    if (munmap(map, stat.st_size) == -1){ 
        close(descrip);
        perror("\nError un-mmapping the file\n");
        exit(EXIT_FAILURE);
    }
    
    pthread_exit(NULL);
}



int nAleatorio(){
    return (rand () % 1001 + 1000);
}

int numAleatorio(){
        return (rand () % 901 + 100);
}