# PRACTICA 1 REDES II
### Autores: Tomas Higuera Viso y Guillermo Hoyo Bravo

## Indice
1. [Diseño del servidor](#diseño)
<<<<<<< HEAD
2. [Funcionalidad extra](#extra)
3. [Uso](#uso)
4. [Referencias](#referencias)
5. [Autores](#autores)
=======
2. [Uso](#uso)
3. [Referencias](#referencias)
4. [Autores](#autores)
>>>>>>> c96f09fd1ef98af5e8d03912fe58f99ec5c7b7d3

## Diseño

### Modelo del servidor
Este servidor esta diseñado para atender lass diferentes peticiones haciendo uso \
de hilos. Cuando llega una peticion se genera un nuevo hilo que es el encargado \
de atenderla y mandar la respuesta al socket. Una vez termina de atender la peticion \
el hilo se destuye. 

Para que el sistema no se sature hemos añadido un parametro en el fichero de \
configuracion del servidor que limita el numero de hilos que se puedan generar \
en el sistema.

### Fichero de configuracion
Nuestro fichero de configuracion del servidor tiene los siguientes campos: \
- server_root: Es la carpeta en la que se encuentran los recursos estaticos del  \
  servidor.
- max_connections: Es el numero maximo de hilos/conexiones al servidor \
- listen_port: Puerto en el que escucha el servidor.
- server_signature: Firma/nombre del servidor.
- server_errors: Carpeta en la que se encuentran los ficheros de error del \
  servidor.
- server_html: Carpeta en la que se encuentra el fichero index.html

### Demonizar
El servidor ejecutara la funcion *to_daemon* que convertira el proceso creado \
a uno en segundo plano desacoplado de la terminal.

### Atender peticiones
Para atender las peticiones hemos hecho uso de la libreria *picohttpparser* que \
dado una peticion http parsea todos los campos a cadenas de caracteres. Si se \
produce algun error se crea la respuesta con el html de error correspondiente \
y se manda al cliente.

El servidor tiene soporte para peticiones de tipo GET, POST y OPTIONS. Si se hace \
alguna peticion que no se corresponde a ninguna de las mencionadas, se lanza \
mensaje de error *not_implemented* junto con el correspondiente html al cliente.

El servidor puede soportar ejecucion de codigo, los parametros vienen dados en \
el cuerpo y la respuesta devuelta por el script es enviada al cliente como cuerpo \
del mensaje http.

<<<<<<< HEAD
###




=======
### Ejecucion de scripts
Cuando se llama a una peticion tipo GET y POST y el recurso solicitado es de tipo \
php o python el codigo se ejecuta en el lado del servidor y se manda la respuesta \
al cliente. Si el tipo de script que necesita ejecutarse no se corresponde con estos \
tipos se manda respuesta de error *not_implemented*.

## Uso
Para ejecutar el servidor es necesario compilarlo haciendo uso del Makefile y basta \
con ejecutar ./server. El fichero de configuracion puede editarse, respetando la \
sintaxis, y se corresponde con server.conf.
>>>>>>> c96f09fd1ef98af5e8d03912fe58f99ec5c7b7d3

## Referencias:
### Referencias templates:
	Templates de error: https://codepen.io/leenalavanya/pen/OogLRd
### Index:
<<<<<<< HEAD
	https://gist.github.com/straker/3c98304f8a6a9174efd8292800891ea1
=======
	https://gist.github.com/straker/3c98304f8a6a9174efd8292800891ea1

## Autores
- Tomas Higuera Viso
- Guillermo Hoyo Bravo
>>>>>>> c96f09fd1ef98af5e8d03912fe58f99ec5c7b7d3
