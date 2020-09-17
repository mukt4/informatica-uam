#ifndef GESTOR_H
#define GESTOR_H

#define TAM_NOMBRE 20
#define MAX_CABALLOS 10
#define MAX_APOSTANTES 50
#define MAX_APUESTAS 20

typedef struct _Gestion{
    int id_cola;
    int id_memoria;
    int id_semaforo;
}Gestion;

/*ESTRUCTURA QUE SE UTILIZARA A LA HORA DE ENVIAR LOS MENSAJES PARA LAS APUESTAS*/
typedef struct _Mensaje{
    long id; /*CAMPO QUE EL ID DE LA APUESTA QUE SE HA REALIZADO*/
    int n_caballo; /*CAMPO QUE CONTIENE EL NUMERO DEL CABALLO POR EL QUE SE APUESTA*/
    double apuesta; /*CANTIDAD APOSTADA POR EL CABALLO*/
    char nombre[TAM_NOMBRE]; /*NOMBRE DEL APOSTADOR QUE REALIZA LA APUESTA*/
    int idApostante; /*CAMPO QUE CONTIENE EL ID DEL APOSTANTE*/
    int nApuesta; /*CAMPO QUE CONTIENE EL NUMERO DE APUESTA DEL APOSTANTE*/
}Mensaje;

/*ESTRUCTURA QUE SE UTILIZARA PARA CONTENET LA INFORMACION QUE COMPARTIRAN LOS PROCESOS*/
typedef struct _Informacion{
    int posicionCaballos[MAX_CABALLOS];
    double cotizacionCaballo[MAX_CABALLOS];
    double dineroApostado[MAX_CABALLOS];
    double totalDinero;
    double totalDineroApostador[MAX_APOSTANTES];
    int idMensaje;
    double ganancia[MAX_APOSTANTES];
    Mensaje apuestas[MAX_APOSTANTES][MAX_APUESTAS];
}Informacion;

/*ESTA ENUMERATION CONTIENE EL TIPO DE TIRADA*/
typedef enum _TipoTirada{
    ESTANDAR,
    GANADORA,
    REMONTADORA
}TipoTirada;

void* gestion(void* gestion);

#endif