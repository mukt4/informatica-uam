/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#include "mapa.h"

struct mapa_ 
{
    // Root de recursos del servidor
    char* root;
    // Carpeta en la que se encuentran ficheros de error
    char* server_errors;
    // Url que se solicita
    char* url;
    // Recurso solicitado
    char* recurso;
    // Nombre del servidor
    char* name;
    // Carpeta en la que se encuentra la carpeta con html
    char* html;
    // Codigo de respuesta
    Cod cod;
    
};

mapa_t* mapa_ini() {
	mapa_t* mapa = (mapa_t*)malloc(sizeof(mapa_t));
    mapa->root = NULL;
    mapa->server_errors = NULL;
    mapa->url = NULL;
    mapa->recurso = NULL;
    mapa->name = NULL;
    mapa->cod = -1;
    mapa->html = NULL;
	return mapa;
}

void mapa_liberar (mapa_t* mapa) {
    if (mapa != NULL) {
        if (mapa->root)
            free(mapa->root);
        if (mapa->server_errors)
            free(mapa->server_errors);
        if (mapa->url)
            free(mapa->url);
        if (mapa->recurso)
            free(mapa->recurso);
        if (mapa->html)
            free(mapa->html);
        free(mapa);
    }
}

char* mapa_get_root (mapa_t* mapa) {
    return mapa->root;
}

char* mapa_get_server_errors (mapa_t* mapa) {
    return mapa->server_errors;
}

char* mapa_get_url (mapa_t* mapa) {
    return mapa->url;
}

char* mapa_get_recurso (mapa_t* mapa) {
    return mapa->recurso;
}

char* mapa_get_html (mapa_t* mapa) {
    return mapa->html;
}

void mapa_set_url (mapa_t* mapa, char* cadena) {
    if (mapa->url != NULL) 
        free(mapa->url);
    mapa->url = strdup(cadena);

}

void mapa_set_recurso (mapa_t* mapa, char* cadena) {
    if (mapa->recurso != NULL) 
        free(mapa->recurso);
    mapa->recurso = strdup(cadena);

}

void mapa_set_html (mapa_t* mapa, char* cadena) {
    if (mapa->html != NULL) 
        free(mapa->html);
    mapa->html = strdup(cadena);

}

void mapa_url_concat (mapa_t* mapa, char* cadena) {
    if (mapa != NULL) {
        if (mapa->url != NULL) {
            size_t size_total = strlen(cadena) + strlen (mapa->url);
            size_total *= sizeof(char);
            mapa->url = (char *) realloc(mapa->url, size_total + 1);
            strcat (mapa->url, cadena);
        }
    }
}

void mapa_set_root (mapa_t* mapa, char* cadena) {
	if (mapa->root != NULL) 
        free(mapa->root);
    mapa->root = strdup(cadena);
}

void mapa_set_name (mapa_t* mapa, char* cadena) {
    if (mapa->name != NULL) 
        free(mapa->name);
    mapa->name = strdup(cadena);
}

char* mapa_get_name (mapa_t* mapa) {
    return mapa->name;
}

void mapa_root_concat (mapa_t* mapa, char* cadena) {
	if (mapa != NULL) {
        if (mapa->root != NULL || cadena != NULL) {
            size_t size_total = strlen(cadena) + strlen (mapa->root);
            size_total *= sizeof(char);
            mapa->root = (char *) realloc(mapa->root, size_total + 1);
            strcat (mapa->root, cadena);
        }
    }
}

void mapa_set_server_errors (mapa_t* mapa, char* cadena) {
    if (cadena == NULL) {
        mapa->server_errors = NULL;
        return;
    }
	if (mapa->server_errors != NULL) 
        free(mapa->server_errors);
    mapa->server_errors = strdup(cadena);
}

Cod mapa_get_cod (mapa_t* mapa) {
    return mapa->cod;
}

void mapa_set_cod (mapa_t* mapa, Cod cod) {
    mapa->cod = cod;
}