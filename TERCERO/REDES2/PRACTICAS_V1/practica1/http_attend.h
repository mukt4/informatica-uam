/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#ifndef HTTPATTEND_H
#define HTTPATTEND_H

#include "unp.h"
#include "types.h"
#include "mapa.h"
#include "picohttpparser.h"

#define MAX_REQ 10000
#define MAX_DATE 100
#define MAX_TAM 1000
#define MAX_HEADERS 100
#define MAX_SCRIPT 200
#define MIN_BODY 2

/***********************************************************************
* Estructura que contendra toda la informacion relativa a una peticion *
************************************************************************/
typedef struct peticion_ peticion_t;


/***************************************************************
* Funcion encargada de escuchar el socket y parsear peticiones *
* Entrada:                                                     *
*       sockfd: Id del socket al que se mandan las peticiones  *                                                     *
* Salida:                                                      *
*       peticion_t: Estructura tipo peticion con toda la       *
*       informacion que se solicita en la request              *
***************************************************************/
peticion_t* parsear_peticion (int sockfd);


/*************************************************************
* Funcion que libera memoria de una estructura tipo peticion *
* Entrada:                                                   *
*       peticion_t: Estructura de tipo peticion que se       *
*       tiene que liberar                                    *
*************************************************************/   
void peticion_liberar (peticion_t* peticion);


/********************************************************************
* Funcion que lanza la respuesta de una peticion tipo GET al socket *
* Entrada:                                                          *
*       sockfd: Id del socket abierto                               *
*       peticion: Estructura que contiene la peticion               *
*       mapa: Estructura tipo mapa con informacion relativa al      *
*       servidor                                                    *
* Salida:                                                           *
*       EXIT_SUCCESS o EXIT_FAILURE                                 *
********************************************************************/
int lanzar_get(int sockfd, peticion_t* peticion, mapa_t* mapa);


/********************************************************************
* Funcion que lanza la respuesta de una peticion tipo que solicita  *
* algun recurso que se tiene que ejecutar en el servidor            *
* Entrada:                                                          *
*       sockfd: Id del socket abierto                               *
*       peticion: Estructura que contiene la peticion               *
*       mapa: Estructura tipo mapa con informacion relativa al      *
*       servidor                                                    *
* Salida:                                                           *
*       EXIT_SUCCESS o EXIT_FAILURE                                 *
********************************************************************/
int lanzar_cgi(int sockfd, peticion_t* peticion, mapa_t* mapa);


/*********************************************************************
* Funcion que lanza la respuesta de una peticion tipo POST al socket *
* Entrada:                                                           *
*       sockfd: Id del socket abierto                                *
*       peticion: Estructura que contiene la peticion                *
*       mapa: Estructura tipo mapa con informacion relativa al       *
*       servidor                                                     *
* Salida:                                                            *
*       EXIT_SUCCESS o EXIT_FAILURE                                  *
*********************************************************************/
int lanzar_post(int sockfd, peticion_t* peticion, mapa_t* mapa);


/*********************************************************************
* Funcion que lanza la respuesta de una peticion mal formada         *
* Entrada:                                                           *
*       sockfd: Id del socket abierto                                *
*       peticion: Estructura que contiene la peticion                *
*       mapa: Estructura tipo mapa con informacion relativa al       *
*       servidor                                                     *
* Salida:                                                            *
*       EXIT_SUCCESS o EXIT_FAILURE                                  *
*********************************************************************/
int lanzar_badreq(int sockfd, peticion_t* peticion, mapa_t* mapa);


/*********************************************************************
* Funcion que lanza la respuesta de un tipo de peticion no           *
* implementada en el servidor                                        *
* Entrada:                                                           *
*       sockfd: Id del socket abierto                                *
*       peticion: Estructura que contiene la peticion                *
*       mapa: Estructura tipo mapa con informacion relativa al       *
*       servidor                                                     *
* Salida:                                                            *
*       EXIT_SUCCESS o EXIT_FAILURE                                  *
*********************************************************************/
int lanzar_not_implemented(int sockfd, peticion_t* peticion, mapa_t* mapa);


/*********************************************************************
* Funcion que imprime una peticion por pantalla                      *
* Entrada:                                                           *
*       peticion: Estructura que contiene la peticion  que se va     *
*       a imprimir                                                   *
*********************************************************************/
void peticion_imprimir(peticion_t* peticion);


/************************************************************
* Funcion que comprueba si el metodo de una peticion es GET *
* Entrada:                                                  *
*       peticion: Peticion que se desea comprobar           *
* Salida:                                                   *
*       Booleano: True si lo es y false si no               *
************************************************************/
Bool peticion_isget(peticion_t* peticion);


/*************************************************************
* Funcion que comprueba si el metodo de una peticion es POST *
* Entrada:                                                   *
*       peticion: Peticion que se desea comprobar            *
* Salida:                                                    *
*       Booleano: True si lo es y false si no                *
*************************************************************/
Bool peticion_ispost(peticion_t* peticion);


/****************************************************************************
* Funcion que manda una respuesta al socket (funcion utilizada por el resto *
* de funciones encargadas de lanzar respuesta)                              *
* Entrada:                                                                  *
*       sockfd: Id del socket donde se mandara la respuesta                 *
*       peticion: Peticion que se parseara para mandar                      *
*       mapa: Mapa que contiene informacion de la localizacion de los       *
*       recursos solicitados en el servidor                                 *
*       optionals: Cadena de caracteres con informacion relativa a la       *   
*       respuesta generada                                                  *
* Salida:                                                                   *
*       Status: ERROR o OK                                                  *
****************************************************************************/
Status enviar_respuesta(int sockfd, peticion_t* peticion, mapa_t* mapa, char* optionals);


/*******************************************************************************
* Funcion encargada de completar los campos de version y fecha de una peticion *
* Entrada:                                                                     *
*       respuesta: String donde se guardara la version y fecha                 *
*       peticion: Peticion que se esta completando                             *
*       mapa: Estructura tipo mapa con informacion relativa a la peticion      *
*       realizada                                                              *
*       optionals: String con informacion relativa a la respuesta              *
*******************************************************************************/
void completar_version_fecha(char* respuesta, peticion_t* peticion, mapa_t* mapa, char* optionals);


/*********************************************************************
* Funcion encargada de completar el tipo de recurso en una respuesta *
* Entrada:                                                           *
*       respuesta: String donde se almacenara el tipo de recurso     *
*       concatenado                                                  *
*       mapa: Mapa que contiene informacion acerca del servidor y    *
*       del recurso solicitado                                       *
*********************************************************************/
void completar_type(char* respuesta, mapa_t* mapa);


/*************************************************************************
* Funcion que completa la respuesta anadiendo el parametro last_modified *
* Entrada:                                                               *
*       respuesta: String donde se concatenara last_modified             *
*       mapa: Mapa con informacion relativa al servidor y al recurso     *
*       solicitado                                                       *
*************************************************************************/
void completar_last_modified(char* respuesta, mapa_t* mapa);

#endif