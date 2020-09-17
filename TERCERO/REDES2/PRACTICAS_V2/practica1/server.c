/**********************                                       *
*   Practica 1: Redes II                                      *
*       Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo    *
*                                                             *
**************************************************************/

#include "server.h"
#include "unp.h"
#include "http_attend.h"
#include "confuse.h"

char CONF_FILE[] = "server.conf";
char* server = NULL;
long int max_conections = -1;
long int listen_port = -1;
char* server_signature = NULL;
char* server_errors = NULL;
char* server_html = NULL;
char abs_path[PATH_MAX];

int main(void){
	int sockfd;
    struct sockaddr_in self;
    int clientfd = -1;
    pthread_t nuevo_hilo;
    struct sockaddr_in client_addr;
    socklen_t addrlen = sizeof(client_addr);

    // Cargamos la confiuguracion del servidor
    cfg_opt_t opts[] = {
        CFG_SIMPLE_STR("server_root", &server),
        CFG_SIMPLE_INT("max_conections", &max_conections),
        CFG_SIMPLE_INT("listen_port", &listen_port),
        CFG_SIMPLE_STR("server_signature", &server_signature),
        CFG_SIMPLE_STR("server_errors", &server_errors),
        CFG_SIMPLE_STR("server_html", &server_html),

        CFG_END()
    };

    cfg_t *cfg;
    cfg = cfg_init(opts, 0);
    cfg_parse(cfg, CONF_FILE);
    
    getcwd(abs_path, sizeof(abs_path));

    // Creamos el socket tipo TCP */
    system("clear");
    to_daemon();
    
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        exit(errno);
    }

    // Inicializamos estructura de dirección y puerto
    bzero(&self, sizeof(self));
    self.sin_family = AF_INET;
    self.sin_port = htons(listen_port);
    self.sin_addr.s_addr = INADDR_ANY;

    // Ligamos puerto al socket
    if (bind(sockfd, (struct sockaddr*)&self, sizeof(self)) != 0){
        printf("bind erroneo\n");
        exit(errno);
    }

    // OK, listos para escuchar...
    if (listen(sockfd, max_conections) != 0){
        printf("listen error\n");
        exit(errno);
    }

    while (TRUE){   
        // Aceptamos conexiones
        clientfd = accept(sockfd, (struct sockaddr*)&client_addr, &addrlen);
        pthread_create(&nuevo_hilo, NULL, &envia_hilo, (void*) &clientfd);

    }

    close(sockfd);
    free(server);
    free(server_signature);
    free(server_errors);
    free(server_html);
    cfg_free(cfg);
    
    return EXIT_SUCCESS;
}

void to_daemon(void) {
    pid_t pid;
    pid_t sid;


    pid = fork();

    if(pid < 0)
        exit(10);

    if(pid > 0) 
    	exit(0);

    /*Estamos en proceso hijo*/

    /*Autorizamos todos los permisos*/
    umask(0);

    /*Abrimos sesión se Syslog*/
    openlog("Server system messages:", LOG_NOWAIT | LOG_PID, LOG_USER);
    syslog(LOG_NOTICE, "Demonio iniciado con éxito\n");


    // Intentar crear una nueva sesión
    sid = setsid();
    if (sid < 0){
        syslog(LOG_ERR, "No fue posible crear una nueva sesión\n");
        exit(11);
    }

    // Cambiar el directorio de trabajo de proceso
    if ((chdir("/")) < 0){
        syslog(LOG_ERR, "No fue posible cambiar el directorio de trabajo a /\n");
        exit(12);
    }

    // Cerrar los descriptores de la E/S estándar
    close(STDIN_FILENO);                    // fd 0
    close(STDOUT_FILENO);                   // fd 1
    close(STDERR_FILENO);                   // fd 2

    /*Como medida de seguridad (si funciones hacen uso de estos descriptores)*/

    open("/dev/null", O_RDONLY);  // fd0 == 0
    open("/dev/null", O_WRONLY);  // fd0 == 1
    open("/dev/null", O_WRONLY);  // fd0 == 2
}

void* envia_hilo(void* arg){
    int value = *(int *)arg;
    mapa_t* mapa = NULL;
    peticion_t* peticion = NULL;

    pthread_detach(pthread_self());
    mapa = mapa_ini();
    mapa_set_root (mapa, abs_path);
    mapa_root_concat (mapa, server);
    mapa_set_server_errors (mapa, server_errors);
    mapa_set_name (mapa, server_signature);
    mapa_set_html (mapa, server_html);

    peticion = parsear_peticion(value);

    if(peticion == NULL) {
        syslog(LOG_ERR, "error peticion\n");
        lanzar_not_implemented(value, peticion, mapa);
    }

    if(peticion_isget(peticion) == TRUE){
        lanzar_get(value, peticion, mapa);  
    }
    else if(peticion_ispost(peticion) == TRUE) {
        lanzar_post(value, peticion, mapa);
    }
    else{ 
        lanzar_badreq(value, peticion, mapa);
    }

    peticion_liberar(peticion);
    mapa_liberar (mapa);
    close(value);

    pthread_exit((void*) 0);
}