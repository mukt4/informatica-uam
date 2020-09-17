/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#ifndef SERVER_H
#define SERVER_H

#include "http_attend.h"
#include "mapa.h"
#include "picohttpparser.h"
#include "unp.h"
#include "types.h"


/*******************************************************
* Funcion que cambia el proceso del servidor a demonio *
*******************************************************/
void to_daemon(void);


/**********************************************************
* Funcion utilizada por los hilos para atender peticiones *
* Entrada:                                                *
*       arg: Informacion adicional para el hilo           *
**********************************************************/
void* envia_hilo(void* arg);

#endif