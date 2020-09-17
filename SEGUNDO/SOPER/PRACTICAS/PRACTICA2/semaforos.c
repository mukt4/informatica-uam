#include "semaforos.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <sys/shm.h>
#include <stdlib.h>


int Inicializar_Semaforo(int semid, unsigned short *copia){
    
    if(copia == NULL || semid == -1)
        return ERROR;
        
    semctl(semid, ((sizeof(copia)) / (sizeof(short))), SETALL, copia);
    
    return OK;
}

int Borrar_Semaforo(int semid){
    if(semid == -1)
        return ERROR;
        
    semctl(semid, 0, IPC_RMID);
    return OK;
}

int Crear_Semaforo(key_t key, int size, int *semid){
    if(!semid)
        return ERROR;
    
    *semid = semget(key, size, IPC_CREAT | IPC_EXCL | SHM_R | SHM_W);
    
    if((*semid == -1) && (errno == EEXIST)){
        *semid = semget(SEMKEY, size, SHM_R | SHM_W);
    }
    
    if(*semid == -1)
        return ERROR;
        
    return OK;
}

int Down_Semaforo(int id, int num_sem, int undo){
    struct sembuf sem_oper;
    
    if(id == -1)
        return ERROR;
    
    sem_oper.sem_num = num_sem;
    sem_oper.sem_op = -1;
    sem_oper.sem_flg = undo;
    
    semop(id, &sem_oper, 1);
    
    return OK;
}

int DownMultiple_Semaforo(int id,int size,int undo,int *active){
    int i;
    struct sembuf sem_oper;
    
    if(active == NULL || id == -1)
        return ERROR;
        
    for(i = 0; i < size; i++){
        sem_oper.sem_num = i;
        sem_oper.sem_op = -1;
        sem_oper.sem_flg = undo;
    
        semop(id, &sem_oper, 1);
    }
    
    return OK;
}

int Up_Semaforo(int id, int num_sem, int undo){
    struct sembuf sem_oper;
    
    if(id == -1)
        return ERROR;
    
    sem_oper.sem_num = num_sem;
    sem_oper.sem_op = 1;
    sem_oper.sem_flg = undo;
    
    semop(id, &sem_oper, 1);
    
    return OK;
}

int UpMultiple_Semaforo(int id,int size, int undo, int *active){
    int i;
    struct sembuf sem_oper;
    
    if(active == NULL || id == -1)
        return ERROR;
        
    for(i = 0; i < size; i++){
        sem_oper.sem_num = i;
        sem_oper.sem_op = 1;
        sem_oper.sem_flg = undo;
    
        semop(id, &sem_oper, 1);
    }
    
    return OK;
}