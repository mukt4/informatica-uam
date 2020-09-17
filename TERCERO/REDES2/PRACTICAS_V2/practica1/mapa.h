/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#ifndef MAPA_H
#define MAPA_H

#include "types.h"
#include "unp.h"

#define MAX_EXT 100

/*****************************************************************
* Estructura tipo mapa que contiene toda la informacion relativa *
* al servidor y al recurso solicitado							 *
*****************************************************************/
typedef struct mapa_ mapa_t;


/******************************************************************
* Funcion que reserva memoria para la estructura mapa e incializa *
* los parametros de la misma 								      *
* Salida:													      *
* 		mapa: Estructura con memoria reservada                    *
******************************************************************/
mapa_t* mapa_ini();


/******************************************************************
* Funcion que se encarga de liberar memoria de la estructura mapa *
* Entrada:														  *
* 		mapa: Estructura que se desea liberar 					  *
******************************************************************/
void mapa_liberar (mapa_t* mapa);


/**************************************************
* Getter del parametro root de la estructura mapa *
* Salida: 									      *
* 		String que contiene el parametro 		  *
**************************************************/
char* mapa_get_root (mapa_t* mapa);


/***********************************************************
* Getter del parametro server_errors de la estructura mapa *
* Salida: 									               *
* 		String que contiene el parametro 		           *
***********************************************************/
char* mapa_get_server_errors (mapa_t* mapa) ;


/**************************************************
* Getter del parametro url de la estructura mapa  *
* Salida: 									      *
* 		String que contiene el parametro 		  *
**************************************************/
char* mapa_get_url (mapa_t* mapa);


/*****************************************************
* Getter del parametro recurso de la estructura mapa *
* Salida: 						   			         *
* 		String que contiene el parametro 		     *
*****************************************************/
char* mapa_get_recurso (mapa_t* mapa);


/*****************************************************
* Getter del parametro name de la estructura mapa    *
* Salida: 						   			         *
* 		String que contiene el parametro 		     *
*****************************************************/
char* mapa_get_name (mapa_t* mapa);


/*****************************************************
* Getter del parametro html de la estructura mapa    *
* Salida: 						   			         *
* 		String que contiene el parametro 		     *
*****************************************************/
char* mapa_get_html (mapa_t* mapa);


/*****************************************************
* Getter del parametro cod de la estructura mapa     *
* Salida: 						   			         *
* 		Cod que contiene el parametro 		         *
*****************************************************/
Cod mapa_get_cod (mapa_t* mapa);

/**********************************************************
* Funcion que concatena cadena y parametro url del mapa   *
* Entrada: 									              *
* 		mapa: Estructura que contiene toda la informacion *
* 		cadena: String con la cadena que se quiere 		  *
* 		concatenar 										  *
**********************************************************/
void mapa_url_concat (mapa_t* mapa, char* cadena);


/***********************************************************
* Funcion que concatena cadena y parametro root del mapa   *
* Entrada: 									               *
* 		mapa: Estructura que contiene toda la informacion  *
* 		cadena: String con la cadena que se quiere 		   *
* 		concatenar 										   *
***********************************************************/
void mapa_root_concat (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro url de la estructura mapa     *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_url (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro root de la estructura mapa    *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_root (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro recurso de la estructura mapa *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_recurso (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro server_errors de la           *
* estructura mapa 									 *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_server_errors (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro name de la estructura mapa    *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_name (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro url de la estructura mapa     *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cadena: String que contiene el parametro     *
*****************************************************/
void mapa_set_html (mapa_t* mapa, char* cadena);


/*****************************************************
* Setter del parametro cod de la estructura mapa     *
* Entrada: 						   			         *
*		mapa: Estructura a la que se setteara        *
* 		la informacion                               *
* 		cod: Codigo de respuesta que se guardara     *
*****************************************************/
void mapa_set_cod (mapa_t* mapa, Cod cod);

#endif
