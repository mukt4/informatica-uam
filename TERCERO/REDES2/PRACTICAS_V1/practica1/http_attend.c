/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#include "http_attend.h"

struct peticion_ {
    // Metodo de la peticion
    char* method;
    // Path que se solicita en la peticion
    char* path;
    // Version
    int version;
    // Tamano de la peticion
    int tamanio;
    // Headers de a peticion
    struct phr_header headers[MAX_HEADERS];
    // Numero de headers de la peticion
    size_t num_headers;
    // Body de la peticion
    char* param;
};

peticion_t* parsear_peticion (int sockfd) {
    int sock=sockfd;
    char buf[MAX_REQ], *method, *path;
    int pret, minor_version;
    size_t buflen = 0, prevbuflen = 0, method_len, path_len;
    ssize_t rret;
    peticion_t* peticion = (peticion_t *)malloc(sizeof(peticion_t));
    if (peticion == NULL) {
        printf("Error de malloc\n");
        return NULL;
    } 

    // Esperamos a poder atender peticion
    while (1) {
        // Leer la peticion
        while((rret = read(sock, buf + buflen, sizeof(buf) - buflen)) == -1 && errno == EINTR);
        
        // IOError;
        if(rret < 0){
            printf("Error de rret\n");
            return NULL;
        }

        prevbuflen = buflen;
        buflen += rret;

        // Parsear la peticion
        peticion->num_headers = sizeof(peticion->headers) / sizeof(peticion->headers[0]);

        pret = phr_parse_request(buf, buflen, (const char**)&method, &method_len,(const char**)&path, &path_len,
                                 &minor_version, peticion->headers, &peticion->num_headers, prevbuflen);
        
        // Si la peticion se ha parseado correctamente
        if(pret > 0)
            break;

        // Error al parsear
        else if(pret == -1){
            printf("error de pret\n");
            return NULL;
        }

        // La solicitud esta incompleta
        assert(pret == -2);

        // La solicitud es demasiado larga
        if(buflen == sizeof(buf)){
            printf("error de buflen\n");
            return NULL;
        }
    }
    if(minor_version > 1){
        printf("minor_version\n");
        return NULL;
    }
    
    // Setteamos los valores de la peticion en la estructura creada
    peticion->version = minor_version;
    peticion->tamanio = pret;
    peticion->method = (char *)malloc((method_len + 1) *sizeof(char));
    peticion->path = (char *)malloc((path_len + 1) *sizeof(char));
    memcpy(peticion->method, method, method_len);
    memcpy(peticion->path, path, path_len);
    peticion->method[method_len] = '\0';
    peticion->path[path_len] = '\0';
    peticion->param = strdup(buf+pret);
    
    return peticion;

}
    
void peticion_liberar (peticion_t* peticion){
	if(peticion){
		if(peticion->method)
			free(peticion->method);
		if(peticion->path)
			free(peticion->path);
        if(peticion->param)
            free(peticion->param);
		free(peticion); 
	}
}

Bool peticion_isget(peticion_t* peticion){
    if(peticion){
        if(strcmp (peticion->method, "GET") == 0)
            return TRUE;
    }

    return FALSE;
}

Bool peticion_ispost(peticion_t* peticion){
    if(peticion){
        if(strcmp (peticion->method, "POST") == 0)
            return TRUE;
    }
    return FALSE;
}

int lanzar_get(int sockfd, peticion_t* peticion, mapa_t* mapa) {
    int sock = sockfd;
    char url [PATH_MAX];
    char recurso [MAX_EXT];
    int i;
    int flag = 0;
    char delimiter[2] = ".";
    char *token = NULL, *pathaux = NULL, *tokenaux = NULL;

    FILE * fp;
    size_t tam = -1;
    
    if(peticion == NULL || mapa == NULL){
        printf("Error paso de parametros (lanzar_get)\n");
        return EXIT_FAILURE;
    }

    bzero(url, PATH_MAX);
    strcat(url, mapa_get_root (mapa));
    mapa_set_url (mapa, url);

    pathaux = (char*)malloc(sizeof(char) * (strlen(peticion->path) + 1));
    strcpy(pathaux, peticion->path);
    token = strtok(pathaux, delimiter);

    while(token != NULL){
        tokenaux = token;
        token = strtok(NULL, delimiter);
    }

    if(strcmp(tokenaux, "py") == 0 || strcmp(tokenaux, "php") == 0){
        lanzar_cgi(sockfd, peticion, mapa);
        return EXIT_SUCCESS;
    }

    // Fichero que devolveremos por defecto (index.html)
    if(strcmp(peticion->path, "/") == 0 || strcmp(peticion->path, "/index.html") == 0){
        mapa_set_recurso (mapa, "html");
        mapa_url_concat (mapa, mapa_get_html(mapa));
        mapa_url_concat (mapa, "/index.html");  
    }

    else{
        mapa_url_concat (mapa, peticion->path);
        tam = strlen(peticion->path);

        for(i = tam; i >= 0 && flag == 0; i--){
            if(peticion->path[i] == '.')
                break;
            if(peticion->path[i] == '/')
                flag = 1;
        }

        bzero(recurso, PATH_MAX);

        if(i != 0 && flag == 0){
            i++;
            memcpy(recurso, peticion->path + i, tam - i);
            mapa_set_recurso (mapa, recurso);

        }

        if(flag == 1){
            mapa_set_cod (mapa, ERROR501);

            // Rehacer la URL
            bzero(url, PATH_MAX);
            strcat(url, mapa_get_root (mapa));
            printf("Ruta sin .\n");
            mapa_set_url (mapa, url);
            mapa_url_concat (mapa, mapa_get_server_errors(mapa));
            mapa_url_concat (mapa, "/e501.html");
            mapa_set_recurso(mapa, "html");
            enviar_respuesta(sock, peticion, mapa, "NOT IMPLEMENTED");
            return EXIT_SUCCESS;
        } 
    }

    fp = fopen (mapa_get_url(mapa), "rb");
    if(fp == NULL){
        mapa_set_cod (mapa, ERROR404);

        // Rehacer la URL
        bzero(url, PATH_MAX);
        strcat(url, mapa_get_root (mapa));
        mapa_set_url (mapa, url);
        mapa_url_concat (mapa, mapa_get_server_errors(mapa));
        mapa_url_concat (mapa, "/e404.html");
        mapa_set_recurso(mapa, "html");
        enviar_respuesta(sock, peticion, mapa, "NOT FOUND");
        return EXIT_SUCCESS;
    }

    fclose(fp);
    mapa_set_cod (mapa, OK200);
    enviar_respuesta(sock, peticion, mapa, "OK");

    return EXIT_SUCCESS;
}

int lanzar_cgi(int sockfd, peticion_t* peticion, mapa_t* mapa){
    int sock = sockfd;
    char url [PATH_MAX];
    char tamanio [MAX_TAM];
    long int tamanio_total = -1;
    char respuesta[MAX_REQ];
    char script[MAX_SCRIPT];
    char buffer[MAX_TAM];
    char recurso [MAX_EXT];
    char* param = NULL;
    char* param_aux = NULL;
    size_t tam = -1;
    int i;
    FILE* fp;
    FILE* retorno;
    int flag = 0;

    if(peticion == NULL || mapa == NULL){
        printf("Error paso de parametros (lanzar_get_param)\n");
        return EXIT_FAILURE;
    }

    //  NO HAY BODY (ESTAMOS EN UN GET)
    if(strlen(peticion->param) < MIN_BODY){
        if(strchr(peticion->path, '?') != NULL){
            // HAY PARAMS POR LA URL
            param = strdup(strchr(peticion->path, '?'));
            strtok(peticion->path, "?");
            param++;
        }
    }

    //  HAY BODY (ESTAMOS EN POST)
    else{
        param_aux = strdup(peticion->param);
        if(strchr(peticion->path, '?') != NULL){
            // HAY BODY Y PARAMS POR LA URL
            param = strdup(strchr(peticion->path, '?'));
            strtok(peticion->path, "?");
            param++;
        }     
    }
    
    mapa_url_concat (mapa, peticion->path);
    tam = strlen(peticion->path);
    for(i = tam; i >= 0 && flag == 0; i--){
        if(peticion->path[i] == '.')
            break;
        if(peticion->path[i] == '/')
            flag = 1;
    }

    bzero(recurso, PATH_MAX);
    if(i != 0 && flag == 0){
        i++;
        memcpy(recurso, peticion->path + i, tam - i);
        mapa_set_recurso (mapa, recurso);
        if((strcmp(recurso,"py") != 0) && (strcmp(recurso,"php") !=0))
            flag = 1;   
    }

    if(flag == 1){
        mapa_set_cod (mapa, ERROR501);
        // REHACER LA URL
        bzero(url, PATH_MAX);
        strcat(url, mapa_get_root (mapa));
        printf("Ruta sin . o no interpretable (no python o php)\n");
        mapa_set_url (mapa, url);
        mapa_url_concat (mapa, mapa_get_server_errors(mapa));
        mapa_url_concat (mapa, "/e501.html");
        mapa_set_recurso(mapa, "html");
        enviar_respuesta(sock, peticion, mapa, "NOT IMPLEMENTED");
        if(param){
            param--;
            free(param);
        } 

        if(param_aux)
            free(param_aux);
        return EXIT_SUCCESS;

    }
    fp = fopen (mapa_get_url(mapa), "rb");
    if(fp == NULL){
        mapa_set_cod (mapa, ERROR404);
        // REHACER LA URL
        bzero(url, PATH_MAX);
        strcat(url, mapa_get_root (mapa));
        mapa_set_url (mapa, url);
        mapa_url_concat (mapa, mapa_get_server_errors(mapa));
        mapa_url_concat (mapa, "/e404.html");
        mapa_set_recurso(mapa, "html");
        enviar_respuesta(sock, peticion, mapa, "NOT FOUND");
        if(param){
            param--;
            free(param);
        }   
        if(param_aux)
            free(param_aux);
        return EXIT_SUCCESS;
    }
    flag = 0;
    fclose(fp);
    mapa_set_cod (mapa, OK200);
    completar_version_fecha(respuesta, peticion, mapa, "OK");
    completar_type(respuesta, mapa);
    completar_last_modified(respuesta, mapa);
    if(strcmp(recurso,"py") == 0){
        /*TIRAR PYTHON*/
    
        //  TENEMOS BODY Y PARAMS POR LA URL
        if(param_aux && param){
            sprintf(script, "echo %s | python %s %s", param_aux, mapa_get_url(mapa), param);
            retorno = popen(script, "r");
            free(param_aux);
        }
        //  TENEMOS PARMS EN BODY SOLO
        else if(param_aux){
            sprintf(script, "python %s %s", mapa_get_url(mapa), param_aux);
            retorno = popen(script, "r");
            free(param_aux);
        }
        //  TENEMOS PARAMS EN URL SOLO
        else{
            sprintf(script, "python %s", mapa_get_url(mapa));
            retorno = popen(script, "r");
        }
        if(retorno == NULL){
            printf("Error al recoger el fichero\n");
            // Tirar 404
            flag = 1;
        }   
    }

    else{
        /*TIRAR PHP*/

        //  TENEMOS BODY Y PARAMS POR LA URL
        if(param_aux && param){
            sprintf(script, "echo %s | php %s %s", param_aux, mapa_get_url(mapa), param);
            retorno = popen(script, "r");
            free(param_aux);
        }
        //  TENEMOS PARMS EN BODY SOLO
        else if (param_aux){
            sprintf(script, "php %s %s", mapa_get_url(mapa), param_aux);
            retorno = popen(script, "r");
            free(param_aux);
        }
        //  TENEMOS PARAMS EN URL SOLO
        else{
            sprintf(script, "php %s", mapa_get_url(mapa));
            retorno = popen(script, "r");
        }
        if(retorno == NULL){
            printf("Error al recoger el fichero\n");
            // Tirar 404
            flag = 1;
        } 
    }
    if(param){
        param--;
        free(param);
    }
        
    if(flag == 1){
        mapa_set_cod(mapa, ERROR501);
        // REHACER LA URL
        bzero(url, PATH_MAX);
        strcat(url, mapa_get_root (mapa));
        printf("Ruta sin . o no interpretable (no python o php)\n");
        mapa_set_url(mapa, url);
        mapa_url_concat(mapa, mapa_get_server_errors(mapa));
        mapa_url_concat(mapa, "/e501.html");
        mapa_set_recurso(mapa, "html");
        enviar_respuesta(sock, peticion, mapa, "NOT IMPLEMENTED");
        return EXIT_SUCCESS;

    }

    tamanio_total = fread(buffer,1,512,retorno);
    sprintf(tamanio, "Content-length: %ld\r\n", tamanio_total);
    strcat(respuesta, tamanio);
    strcat(respuesta, "\r\n");
    send(sock, respuesta, strlen(respuesta), MSG_MORE);
    send(sock, buffer, strlen(buffer), 0);
    fclose(retorno);
    
    return EXIT_SUCCESS;;
}

int lanzar_post(int sockfd, peticion_t* peticion, mapa_t* mapa) {
    int sock = sockfd;
    char url [PATH_MAX];

    if(peticion == NULL || mapa == NULL){
        printf("Error paso de parametros (lanzar_post_param)\n");
        return EXIT_FAILURE;
    }
    bzero(url, PATH_MAX);
    strcat(url, mapa_get_root (mapa));
    mapa_set_url (mapa, url);
    lanzar_cgi(sock, peticion, mapa);
    
    return EXIT_SUCCESS;
}

int lanzar_badreq(int sockfd, peticion_t* peticion, mapa_t* mapa){
    int sock = sockfd;
    char url [PATH_MAX];

    bzero(url, PATH_MAX);
    strcat(url, mapa_get_root (mapa));
    mapa_set_url (mapa, url);
    mapa_url_concat (mapa, mapa_get_server_errors(mapa));
    mapa_url_concat (mapa, "/e400.html");
    mapa_set_recurso(mapa, "html");
    mapa_set_cod (mapa, ERROR400);
    enviar_respuesta(sock, peticion, mapa, "BAD REQUEST");

    return EXIT_SUCCESS;
}

int lanzar_not_implemented(int sockfd, peticion_t* peticion, mapa_t* mapa){
    int sock = sockfd;
    char url [PATH_MAX];

    bzero(url, PATH_MAX);
    strcat(url, mapa_get_root (mapa));
    mapa_set_url (mapa, url);
    mapa_url_concat (mapa, mapa_get_server_errors(mapa));
    mapa_url_concat (mapa, "/e501.html");
    mapa_set_recurso(mapa, "html");
    mapa_set_cod (mapa, ERROR501);
    enviar_respuesta(sock, peticion, mapa, "NOT IMPLEMENTED");

    return EXIT_SUCCESS;
}

void peticion_imprimir(peticion_t* peticion){
    if(peticion){
        if(peticion->method)
            fprintf(stdout, "Metodo: %s\n", peticion->method);
        if(peticion->path)
            fprintf(stdout, "Path: %s\n", peticion->path);
    }
}

Status enviar_respuesta(int sockfd, peticion_t* peticion, mapa_t* mapa, char* optionals){
    char buffer[MAX_TAM];
    int sock = sockfd;
    char tamanio [MAX_TAM];
    long int tamanio_total = -1;
    FILE* pf;
    size_t bread = -1; 
    char respuesta[MAX_REQ];

    completar_version_fecha(respuesta, peticion, mapa, optionals);
    completar_type(respuesta, mapa);
    completar_last_modified(respuesta, mapa);
    pf = fopen (mapa_get_url(mapa), "rb");
    fseek(pf, 0L, SEEK_END);
    tamanio_total = ftell(pf);
    fseek(pf, 0L, SEEK_SET);   

    sprintf(tamanio, "Content-length: %ld\r\n", tamanio_total);
    strcat(respuesta, tamanio);
    strcat(respuesta, "\r\n");
    send(sock, respuesta, strlen(respuesta), MSG_MORE);

    while(!feof(pf)){
        bzero(buffer, MAX_TAM);
        bread = fread(buffer, 1, sizeof(buffer), pf);
        if(feof(pf))
           send(sock, buffer, bread, 0); 
        send(sock, buffer, bread, MSG_MORE);
    }    
    
    fclose(pf);

    return (EXIT_SUCCESS);
}

void completar_version_fecha(char* respuesta, peticion_t* peticion, mapa_t* mapa, char* optionals){
    char aux[MAX_HEADERS];
    char date[MAX_DATE];
    time_t t;
    struct tm *tmp;
    time( &t );

    bzero(respuesta, MAX_REQ);
    bzero(date, MAX_DATE);
    bzero(aux, MAX_HEADERS);

    sprintf(aux, "Server: %s\r\n", mapa_get_name (mapa));

    tmp = localtime(&t);

    if(peticion == NULL)
        sprintf(respuesta, "HTTP/1.1 %d %s\r\n", mapa_get_cod (mapa), optionals);
    else
        sprintf(respuesta, "HTTP/1.%d %d %s\r\n", peticion->version, mapa_get_cod (mapa), optionals);
    
    // Strftime para poder imprimir la hora correctamente
    strftime(date, sizeof(date), "%a, %d %b %Y %X GMT", tmp);
    strcat(respuesta, "Date: ");
    strcat(respuesta, date);
    strcat(respuesta, "\r\n");
    strcat(respuesta, aux);
}

void completar_type(char* respuesta, mapa_t* mapa){
    if(!strcmp(mapa_get_recurso(mapa), "html") || !strcmp(mapa_get_recurso(mapa), "htm")) 
        strcat(respuesta, "Content-type: text/html\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "gif"))
        strcat(respuesta, "Content-type: image/gif\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "jpeg") || !strcmp(mapa_get_recurso(mapa), "jpg"))
        strcat(respuesta, "Content-type: image/gif\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "mpeg") || !strcmp(mapa_get_recurso(mapa), "mpg"))
        strcat(respuesta, "Content-type: video/mpeg\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "doc") || !strcmp(mapa_get_recurso(mapa), "docx"))
        strcat(respuesta, "Content-type: application/msword\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "pdf"))
        strcat(respuesta, "Content-type: application/pdf\r\n");
    else if(!strcmp(mapa_get_recurso(mapa), "js"))
        strcat(respuesta, "Content-type: text/jscript\r\n");
    else
        strcat(respuesta, "Content-type: text/plain\r\n");
}

void completar_last_modified(char* respuesta, mapa_t* mapa){
    char date[MAX_DATE];
    char buffer[MAX_HEADERS];
    struct tm * stm;
    struct stat c;

    bzero(date, MAX_DATE);
    bzero(buffer, MAX_HEADERS);

    if(stat(mapa_get_url (mapa), &c) == 0){
        stm = gmtime(&c.st_mtime);
        strftime(date, sizeof(date), "%a, %d %b %Y %X GMT", stm);
        sprintf(buffer, "Last-Modified: %s\r\n", date);
        strcat(respuesta, buffer);
    }
}