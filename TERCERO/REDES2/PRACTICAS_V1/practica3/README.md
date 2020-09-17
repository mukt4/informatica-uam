# PRACTICA 3 REDES II
### Autores: Tomas Higuera Viso

## Indice 
1. [Diseño](#diseno)
2. [Dependencias](#dependencias)
3. [Uso](#uso)
4. [Funcionalidad extra](#extra)
5. [Autor](#autor)


## 1. Diseño <a name="diseno"></a>
En esta practica hemos optado por un diseño basado en hilos concurrentes que \
gestionene cada uno una pequeña parte de la funcionalidad total.
Los modulos creados son los siguientes:
- Prot_conexion: Encagardo de la comunicacion con el servidor de descubrimiento
- Prot_transporte Encargado de la gestion de llamada y de los hilos involucrados \
en la misma.

Al iniciar la aplicacion se crea una instancia de los objetos Prot_conexion y Prot_transporte \

El objeto Prot_conexion tiene un socket abierto para comunicarse con el servidor \
de descubrimiento. Ademas tiene los atributos de la IP y puerto de escucha, asi \
como la informacion del usuario.

El objeto Prot_transporte crea dos sockets escuchadno en el puerto establecido por \
Prot_conexion. Uno de ellos se encarga de estar a la espera de que se conecte otro usuario \
y el otro esta a la espera de que se establezca la llamada para recibir los frames de \
video.

La aplicacion al iniciarse genera tres hilos. Dos de ellos van a cargo de la libreria appJar \
y son los encargados de gestionar la interfaz grafica. El tercer hilo se encarga de \
ejecutar la funcion atenderLlamada. Esta funcion se bloquea en un accept() a la espera \
de clientes y dependiendo de si el usuario pendiente de aceptacion deniega la llamada.

Cuando queremos hacer una llamada pulsamos el boton de conectar y nos aparece un listado con \
todos los posibles usuarios que podemos llamar. De entre estos usuarios seleccionamos uno \
y pulsamos conectar.
En este momento si el usuario al que llamamos esta conectado lle aparecera un mensaje \
con la posibilidad de unirse a la llamada. Si este usuario acepta comienza la creacion de hilos.

### Llamante
El hilo que ejecuta la funcion de comenzarLlamada abre los sockets correspondientes \
y crea 4 hilos mas. Dos de ellos se encargan del envio y recibo de datos mediante \
UDP. Los otros dos son los encagrados de gestionar el protocolo de llamada. Uno encargado \
de enviar los datos por el TCP y otro de recibirlos.

### Llamado
En caso de aceptar la llamada, esta es aceptada desde el hilo que estaba a la espera de clientes. \
Este hilo crea un hilo encargado de al igual que desde el punto de vista del llamante de crear \
otros 4 hilos y de esperarlos cuando acabe la llamada. El hilo que ha realizado el accept \
no realiza el join del hilo que ha creado porque es independiente y se vuelve a quedar \
bloqueado a la espera de otros clientes para rechazarles la llamada de forma automática.

## Máquina de estados para gestionar el estado de las llamadas
Todos los hilos y estado de la aplicación dependen de dos variables que se utilizan \
para determinar si estamos en una llamada o no y en que estado de la llamada nos encontramos. \
La variable llamadaEstablecida determina si la aplicación se encuentra en una llamada o no. \
La variable estadoLlamada es la encargada de determinar si la llamada se encuentra \
activa, detenida o en proceso de ser colgada.
El acceso a las variables anteriores se encuentra protegido por diferentes semáforos \
mutex para evitar bloqueos.

## 2. Dependencias <a name="dependencias"></a>
Se incluye un fichero de *requirements.txt* con todas las dependencias de la \
aplicacion. Para instalarlas sera necesario ejecutar *pip install -r requirements.txt*.

## 4. Funcionalidad extra <a name="extra"></a>
La funcionalidad extra que he anadido ha sido la posibilidad de, al igual que haciamos con las \
llamadas, poder iniciar un chat a traves de mensajes con otro usuario de la aplicacion. \
Para esto he anadido algunos comandos nuevos a la aplicacion que son los siguientes: \
- MESSAGE + nick + puerto : Este comando se utiliza para iniciar un chat a traves de mensajes \
con el otro usuario
- MESSAGE_ACEPTED: Este comando se utiliza para aceptar un chat de mensajes con el otro usuario.
- MESSAGE_DENIED: Este comando se utiliza para rechazar un chat de mensajes con el otro usuario.
- MESSAGE_END: Este comando se utiliza para finalizar un chat de mensajes con el otro usuario.

Finalmente no he conseguido hacer funcionar el chat por mensajes, he tenido un error en los hilos \
que no he sido capaz de resolver.

## 5. Uso <a name="uso"></a>
Para ejecutar la aplicacion tan solo es necesario instalar las dependencias y \
ejecutar *./video_client.py*.

## Autor <a name="autor"></a>
Tomas Higuera Viso



