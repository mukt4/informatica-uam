# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import re
import os
import argparse
from encryptManager import EncryptManager
from identity import Identity
from usersAPI import UsersAPI

"""
CLASS NAME: AppAction
Clase de la que heredaran el resto de acciones. Esta clase sirve apara gestionar los diferentes tipos
de acciones haciendo uso de argparse.
"""
class AppAction(argparse.Action):
	"""
	NAME: __init__(constructor)
	DEFINITION: Este es el constructor de la clase AppAction, que ademas de incializar la clase,
				creara el resto de clases que seran usadas en la aplicacion.
	PARAMETERS: option_strings
				dest
				**kwargs 
	RETURN: Instancia de la clase AppAction creada
	"""
	def __init__(self, option_strings, dest, **kwargs):
		# Inicializamos la clase que se encarga de realizar los cifrados
		self.encryptManager = EncryptManager()
		# Inicializamos la clase que se encarga de la gestion de sesiones
		self.usersAPI = UsersAPI()
		# Si se encuentra un fichero de identidad se carga en el sistema
		if os.path.isfile("userData.bin"):
			self.identity = Identity.loadIdentity("userData.bin")
			if self.identity == None:
				print('Contrasena incorrecta')
				exit()
		else:	
			self.identity = None
		super(AppAction, self).__init__(option_strings, dest, **kwargs)

"""
CLASS_NAME: CreateAction
Clase que hereda de AppAction y se encarga de crear una identidad con un email y un usuario 
"""
class CreateAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values=None, option_string=None):
		if(re.match("[^@]+@[^@]+\.[^@]+", values[1])):
			# Guardamos el nombre y el email
			name = values[0]
			email = values[1]
			print("Creating user with name '" + name + "' and email: '" + email + "'...")

			# Generamos las claves publica y privada que usuaremos
			keys = self.encryptManager.createKeys()
			# Generamos la nueva identidad
			newIdentity = Identity(name, email, keys)
			# Guardamos la identidad en un fichero
			newIdentity.exportIdentity("userData.bin")
			# Publicamos la identidad en el servidor
			self.usersAPI.registerUser(name, email, newIdentity.getPublicKey())

		else:
			print("Malformed email")


"""
CLASS_NAME: SearchAction
Clase que hereda de AppAction y se encarga de buscar un usuario en el servidor
"""
class SearchAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Guardamos la id que vamos a buscar
		userID = values
		print("Searching user '" + userID + "' in the server...")
		self.usersAPI.searchUser(userID)

"""
CLASS_NAME: DeleteAction
Clase que hereda de AppAction y se encarga de eliminar un usuario del servidor
"""
class DeleteAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Comprobamos si existe un fichero de usuario en la aplicacion
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option.")
			return

		userID = values
		print("Deleting user with id '" + userID + "' in the server...")
		self.usersAPI.deleteUser(userID)
 
"""
CLASS_NAME: ListAction
Clase que hereda de AppAction y se encarga de listar todos los ficheros del servidor de un usuario
"""
class ListAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Comprobamos si existe un fichero de usuario en la aplicacion
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option")
			return

		print("Listing files in the server...")
		self.usersAPI.listFiles()
 
"""
CLASS_NAME: SourceIdAction
Clase que hereda de AppAction y se encarga de descargar un fichero de la aplicacion
"""
class SourceIdAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option.")
			return
		# El fichero que se desea descargar se encuentra en send
		if(namespace.download):
			# Guardamos la id del fichero que vamos a descargar
			fileID = namespace.download
			# Guardamos la id del emisor del mensaje
			src_id = values
			# Antes comprobar si existe el fichero
			print("Downloading file with ID: " + fileID + "...")
			# Descargamos el fichero con el identificador proporcionado
			downloadedFile = self.usersAPI.downloadFile(fileID)

			# Obtenemos la clave publica del destinatario
			clavePublica = self.usersAPI.obtenerPublicKey(src_id)

			# Tras obtener el mensaje y la clave publica del emisor desencriptamos y comprobamos la firma
			resultadoDescifrar = self.encryptManager.descifrar_mensaje(fileID, downloadedFile, clavePublica, self.identity.getPrivateKey())
			# Comprobamos si hemos podido descifrar el fichero descargado
			if resultadoDescifrar == -1:
				# Si la firma digital no es correcta no se genera el fichero descargado
				print("The digital sign could not be verified. The file wont be decypher.")
			else:
				# Si la firma digital es correcta si se genera el fichero 
				print("La firma digital ha sido verificada, el fichero descifrado es decrypt_" + fileID)
			return
		else:
			print("Arguments not setted correctly")

"""
CLASS_NAME: DeleteFileAction
Clase que hereda de AppAction y se encarga de eliminar un fichero del servidor con una id concreta
"""
class DeleteFileAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		id = values
		print("Deleting file with ID: '" + id + "'...")
		self.usersAPI.deleteFile(id)

"""
CLASS_NAME: SignAction
Clase que hereda de AppAction y se encarga de firmar un fichero
"""
class SignAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Setteamos el nombre del fichero
		file = values
		# Comprobamos si existe un fichero de usuario en la aplicacion
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option.")
			return

		print("Signing file '" + file + "'...")

		# Creamos la firma del fichero que vamos a mandar (con nuestra clave privada)
		firma = self.encryptManager.crearFirma(file, self.identity.getPrivateKey())

		# Abrimos el archivo que contiene el mensaje y lo leemos
		fichero = open(file, "rb")
		mensaje = fichero.read()
		fichero.close()

		# Escribimos el fichero en el que se guardara la firma. Primero la firma y luego el mensaje
		signedFile = open("signed_" + file, "wb")
		signedFile.write(firma + mensaje)
		signedFile.close()

		# Informamos del fichero generado
		print()
		print("File signed and saved in signed_" + file)
		print()

"""
CLASS_NAME: InfoAction
Clase que hereda de AppAction y se encarga de mostrar la informacion del usuario actual
"""
class InfoAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Comprobamos si existe un fichero de usuario en la aplicacion
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option.")
			return

		print('Info about user logged in: ')
		print('Name: ' + self.identity.getName())
		print('Email: ' + self.identity.getEmail())


"""
CLASS_NAME: DestIdAction
Clase que hereda de AppAction y se encarga de subir un fichero a la aplicacion
"""
class DestIdAction(AppAction):
	"""
	NAME: __call__
	DEFNITION: Funcion a la que se accede cuando la accion es llamada
	PARAMETERS: parser
				namespace
				values - Argumentos de entrada
				option_string
	RETURN: void
	"""
	def __call__(self, parser, namespace, values, option_string=None):
		# Comprobamos si existe un fichero de usuario en la aplicacion
		if self.identity == None:
			print("ERROR: No identity associated, use create_id option.")
			return

		# Guardamos la id de destino
		dest_id = values
		# Se quiere hacer operacion de cifrado
		if(namespace.encrypt):
			# Guardamos el nombre del fichero
			file = namespace.encrypt
			print("Encrypting file '" + file + "' using public key from '" + dest_id + "'...")
			# Obtenemos la clave publica del destinatario
			clavePublica = self.usersAPI.obtenerPublicKey(dest_id)

			# Generamos el mensaje cifrado pero sin firma con la clave publica del destinatario
			msgEncriptado = self.encryptManager.encriptar(file, clavePublica)

			# Abrimos el fichero y escribimos el mensaje cifrado
			signedFile = open("encrypted_nosigned_" + file, "wb")
			signedFile.write(msgEncriptado)
			signedFile.close()

			# Informamos del nombre del nuevo fichero generado
			print()
			print("File encrypted and saved in encrypted_nosigned_" + file)
			print()

		# Se quiere hacer operacion de firma y cifrado
		elif(namespace.enc_sign):
			# Guardamos el nombre del fichero
			file = namespace.enc_sign
			print("Encrypting and signing file '" + file + "' using public key from '" + dest_id + "'...")
			# Creamos la firma del documento que vamos a mandar (con nuestra clave privada)
			firma = self.encryptManager.crearFirma(file, self.identity.getPrivateKey())

			# Obtenemos la clave publica del destinatario con la que cifraremos el mensaje
			clavePublica = self.usersAPI.obtenerPublicKey(dest_id)

			# Generamos el mensaje cifrado y firmado
			msgEncriptado = self.encryptManager.generar_mensaje_cifrado(file, firma, clavePublica)

			# Escribimos el fichero en el que se guardara el mensaje cifrado y firmado
			file_toSend = open("encrypted_signed_" + file, "wb")
			file_toSend.write(msgEncriptado)
			file_toSend.close()

			# Informamos del nombre del fichero cifrado y firmado generado
			print()
			print("File signed, encrypted and saved in encrypted_signed_" + file)
			print()

		# Se quiere firmar, cifrar y subir al servidor
		elif(namespace.upload):
			# Guardamos el nombre del fichero
			file = namespace.upload
			print("Uploading file '" + file + "' using public key from '" + dest_id + "'...")
			# Creamos la firma del documento que vamos a mandar (con nuestra clave privada)
			firma = self.encryptManager.crearFirma(file, self.identity.getPrivateKey())

			# Obtenemos la clave publica del destinatario
			clavePublica = self.usersAPI.obtenerPublicKey(dest_id)

			# Generamos el mensaje cifrado
			msgEncriptado = self.encryptManager.generar_mensaje_cifrado(file, firma, clavePublica)

			# Guardamos el mensaje cifrado en un nuevo fichero
			file_toSend = open("encrypted_" + file, "wb")
			file_toSend.write(msgEncriptado)
			file_toSend.close()

			#Hay que llamar a la funcion de uploadFile en UsersAPI para subir el fichero cifrado al servidor
			self.usersAPI.uploadFile("encrypted_" + file)
		else:
			print("Arguments not setted correctly")
