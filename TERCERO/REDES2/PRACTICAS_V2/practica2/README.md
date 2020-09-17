# PRACTICA 2 REDES II
### Autor: Guillermo Hoyo Bravo

## Indice 
1. [Diseño](#diseno)
    1. [Cliente](#cliente)
    2. [Criptografia](#criptografia)
2. [Dependencias](#dependencias)
3. [Uso](#uso)
4. [Funcionalidad extra](#extra)
5. [Autores](#autores)


## Diseño <a name="diseno"></a>
### Cliente <a name="cliente"></a>
Para el desarrollo de esta practica hemos optado por realizar un diseño orientado \
a objetos. El objeto que se encarga de gestionar todas las posibles acciones es \
*SecureBoxClient*.

### SecureBoxClient
Este objeto contiene las posibles acciones del sistema. Para gestionar la interaccion \
del usuario con el mismo hemos hecho uso de agrparse. Cada posible accion en el sistema \
esta gestionada por un objeto de tipo *argparse.Action*. Los diferentes objetos son \
los siguientes:

#### CreateAction
Crea una identidad con un nombre y email en el servidor.

#### SearchAction
Busca un usuario con un determinado id en el servior.

#### DeleteAction
Elimina un usuario con un determinado id del servidor.

#### ListAction
Lista todos los ficheros que un usuario tiene subidos al servidor.

#### SourceIdAction
Descarga un fichero con un determinado id del servidor.

#### DeleteFileAction
Borra un fichero con un determinado id del servidor.

#### SignAction
Firma un fichero.

#### DestIdAction
Sube un fichero al servidor.

### EncryptManager
Este objeto se encarga de realizar todas las acciones con firma, cifrado y \
descifrado de ficheros. Este objeto contiene los siguientes metodos:
```python
createKeys(self) # Genera claves publico/privada.
crearFirma(self, file, key) # Genera firma de un fichero
encriptar(self, file, publicKey) # Cifra un fichero
generar_mensaje_cifrado(self, file, firma, publicKey) # Firma y cifra un fichero
descifrar_mensaje(self, fichero, contenido, clavePublica, clavePrivada) # Descifra
```

### UsersAPI
Este objeto es el encargado de comunicarse con el servidor y realizar las diferentes \
acciones de la aplicacion. Este objeto contiene los siguientes metodos:
```python
registerUser(self, name, email, publicKey) # Registra usuario en el servidor
obtenerPublicKey(self, userID) # Obtiene publicKey de un usuario
searchUser(self, userID) # Busca un usuario en el servidor
deleteUser(self, userID) # Borra un usuario del servidor
listFiles(self) # Lista los ficheros de un usuario del servidor
uploadFile(self, file) # Sube un fichero al servidor
downloadFile(self, fileID) # Descarga un fichero del servidor
deleteFile(self, file_id) # Elimina un fichero del servidor
```

### Identity
Este objeto es el encargado de gestionar las identidades de la aplicacion. Contiene \
toda la informacion acerca del usuario actual. Este objeto contiene los siguientes \
metodos:
```python
loadIdentity(fileName) # Carga una identidad desde un fichero
exportIdentity(self, fileName) # Exporta la identidad actual a un fichero
getPublicKey(self) # Obtiene clave publica del usuario actual
getPrivateKey(self) # Obtiene claver privada del usuario actual
getName(self) # Obtiene el nombre del usuario actual
getEmail(self) # Obtiene el email del usuario actual
```

## Criptografia <a name="criptografia"></a>
Para subir un archivo al servidor es necesario firmar y cifraro antes para poder \
garantizar autenticidad, confidencialidad e integridad. El proceso que se sigue es \
el siguiente:
1. *Firmar*: Se calcula el has del mensaje con SHA1 de 256 bits y se cifra con la \
clave RSA privada del emisor.
2. *Cifrar*: Generamos una clave aleatoria y con cifrado simetrico(AES) ciframos la \
firma y el mensaje.
3. *Mensaje final*: Concatenamos el IV a lo cifrado con AES y a eso le concatenamos \
la clave simetrica cifrada con RSA con la clave publica del receptor.

La estructura final es la siguiente:
```
ClaveAESCifrada(256 bits) + [IV(16 bits) + [FirmaCifradaRSA(256 bits) + Mensaje ] ]
```

### Consideraciones firma y cifrado
- *Longitud de clave RSA*: Algoritmo de claves de 2048 bits.
- *Clave AES*: Clave de 256 bits.
- *Vector de incializacion(IV)*: Vector de 128 bits.
- *Tamaño de los bloquues CBC*: El cifrado simetrico se hace en bloques de 32 bytes
- *Algoritmo AES*: Ultilizamos el modo CBC de AES.

## Dependencias <a name="dependencias"></a>
Se incluye un fichero de *requirements.txt* con todas las dependencias de la \
aplicacion. Para instalarlas sera necesario ejecutar *pip install -r requirements.txt*.

## Uso <a name="uso"></a>
Para ejecutar la aplicacion tan solo es necesario instalar las dependencias y \
ejecutar *./securebox_client.py*. Al ejecutar sin argumentos se mostraran todas \
las posibles acciones.

## Funcionalidad extra <a name="extra"></a>
La funcionalidad que he anadido a esta practica ha sido cifrar el fichero en el que almaceno \
la informacion del usuario con AES. Con esto consigo garantizar la identidad del usuario que \
utiliza la aplicacion. Antes de la mejora si alguien tenia el fichero binario con las claves \
he informacion de usuario podia suplantar tu identidad. Con esta mejora conseguimos que a pesar \
de tener este fichero sea necesario el uso de una contrasena para hacer uso de la aplicacion.

## Autores <a name="autores"></a>
Tomas Higuera Viso y Guillermo Hoyo Bravo



