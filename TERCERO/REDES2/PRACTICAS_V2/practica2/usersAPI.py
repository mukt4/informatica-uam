# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import requests
import json
import datetime
import urllib3
import os
import sys

"""
CLASS_NAME: UsersApi
Esta clase se encargara de mantenr la conexion con el servidor SECUREBOX
"""
class UsersAPI:
	"""
	NAME: __init__(constructor)
	DEFINITION: Constructor de la clase UsersApi
	PARAMETERS: None
	RETURN: Instancia de UsersApi creada
	"""
	def __init__(self):
		self.secureBoxURL = "https://vega.ii.uam.es:8080"
		self.authorization = "Bearer eE238bdD764BFfc0" # Token de Guillermo
							 
		# Desactivamos warnings de SSL al conectarse por HTTPS
		urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

	"""
	NAME: registerUser
	DEFINITION: Funcion que registra un usuario en el servidor
	PARAMETERS: name - Nombre del usuario que se desea registrar
				email - Email del usuario que se desea registrar
				publicKey - Clave publica del usuario
	RETURN: void
	"""
	def registerUser(self, name, email, publicKey):
		# Generamos la solicitud
		url = self.secureBoxURL + "/api/users/register"
		headers = {"Authorization": self.authorization}
		args = {"nombre": name, "email": email, "publicKey": publicKey.decode("utf-8")}
		# Realizamos la solicitud y guardamos la respuesta
		response = requests.post(url, headers=headers, json=args, verify=False)
		# Parseamos la respuesta
		jsonResponse = json.loads(response.text)
		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			# Obtenemos la marca de tiempo en la que se ha generado el mensaje
			time = datetime.datetime.fromtimestamp(jsonResponse["ts"]).strftime("%H:%M:%S %Y-%m-%d")
			# Imprimimos mensaje de confirmacion
			print(time + ": User created as " + jsonResponse["userID"])
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()

	"""
	NAME: obtenerPublicKey
	DEFINITION: Funcion que obtiene la public key de un usuario del servidor
	PARAMETERS: userId - Id del usuario del que queremos obtener la clave publica
	RETURN: void
	"""
	def obtenerPublicKey(self, userID):
		# Generamos la solicitud
		url = self.secureBoxURL + "/api/users/getPublicKey"
		headers = {"Authorization" : self.authorization}
		args = {"userID" : userID}

		# Realizamos la solicitud y guardamos la respuesta
		response = requests.post(url, headers=headers, json=args, verify=False)
		# Parsamos la respuesta
		jsonResponse = json.loads(response.text)

		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			# Si consigue acceder devolvemos la publicKey encontrada
			return jsonResponse["publicKey"].encode("utf-8");
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()

	"""
	NAME: searchUser
	DEFINITION: Funcion que busca un usuario con un id en el servidor
	PARAMETERS: userId - Id del usuario que se desea buscar
	RETURN: void
	"""
	def searchUser(self, userID):
		# Generamos la solicitud
		url = self.secureBoxURL + "/api/users/search"
		headers = {"Authorization" : self.authorization}
		args = {"data_search" : userID}

		# Realizamos la solicitud y guardamos la respuesta
		response = requests.post(url, headers=headers, json=args, verify=False)
		# Parseamos la respuesta
		jsonResponse = json.loads(response.text)
		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			# Por cada usuario se imprime su informacion desglosada
			usersFound = 0
			for user in jsonResponse:
				print()
				print("ID: " + str(user["userID"]))
				print("Nombre: " + str(user["nombre"]))
				print("Email: " + str(user["email"]))
				print("Public Key: ")
				print(str(user["publicKey"]))
				print()
				usersFound = 1

			# Si no se han encontrado usuarios en el servidor se informa de ello
			if usersFound == 0:
				print("User not found in the server.")
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()

	"""
	NAME: deleteUser
	DEFINITION: Funcion que elimina un usuario con un id del servidor
	PARAMETERS: userId - Id del usuario que se desea eliminar
	RETURN: void
	"""
	def deleteUser(self, userID):
		# Generemos la solicitud
		url = self.secureBoxURL + "/api/users/delete"
		headers = {"Authorization": self.authorization}
		args = {"userID": userID}

		# Si existe un fichero con los datos de identificacion, se borra
		if os.path.isfile("userData.bin"):
			os.remove("userData.bin")

		# Realizamos la solicitud y guardamos la respuesta
		response = requests.post(url, headers=headers, json=args, verify=False)
		# Parseamos la respuesta
		jsonResponse = json.loads(response.text)
		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			# Imprimimos mensaje de confirmacion
			print("User ID: " + jsonResponse["userID"] + " deleted.")
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()


	"""
	NAME: listFiles
	DEFINITION: Funcion que lista todos los ficheros del servidor del usuario actual de la aplicacion
	PARAMETERS: None
	RETURN: void
	"""
	def listFiles(self):
		# Generamos la solicitud
		url = self.secureBoxURL + "/api/files/list"
		headers = {"Authorization": self.authorization}

		# Realizamos la solicitud
		response = requests.post(url, headers=headers, verify=False)
		# Parseamos la respuesta
		jsonResponse = json.loads(response.text)
		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			# Extraemos el numero de ficheros devueltos, si es 0, informamos de que no hay ficheros
			if jsonResponse["num_files"] == 0:
				print("User have no files in the server.")
				return
			print("List of files of the user: ")
			i = 0
			# Vamos recorriendo la informacion de cada fichero y extrayendo su ID y su nombre
			for datos_fichero in jsonResponse["files_list"]:
				print()
				i = i + 1
				print("File: " + str(i))
				print("Name: " + datos_fichero["fileName"])
				print("Identifier: " + datos_fichero["fileID"])
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()

	"""
	NAME: uploadFile
	DEFINITION: Funcion que sube un fichero al servidor
	PARAMETERS: file - Fichero que se desea subir al servidor
	RETURN: void
	"""
	def uploadFile(self, file):
		# Generamos la solicitud
		url = self.secureBoxURL + "/api/files/upload"
		headers = {"Authorization": self.authorization}
		# Abrimos un descriptor de fichero con el fichero a subir
		fileToUpload = {"ufile": open(file, "rb")}
		
		# Realizamos la solicitud 
		response = requests.post(url, headers=headers, files=fileToUpload, verify=False)
		# Parseamos la respuesta
		jsonResponse = json.loads(response.text)

		# Tratamos de acceder a los campos de la respuesta, si no se puede, es que se ha producido un error
		try:
			tamanoArchivo = jsonResponse["file_size"]
			idArchivo = jsonResponse["file_id"]
			# Si podemos acceder a los campos de la respuesta quiere decir que se ha subido correctamemte,
			# informamos de la subida con los parametros de la subida (tamano e ID)
			print("File of size " + str(tamanoArchivo) + " bytes uploaded with ID: " + str(idArchivo) + " to the server.")
		except:
			# Si no, imprimimos un mensaje de error informando de la causa de error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])
			sys.exit()

	"""
	NAME: downloadFile 
	DEFINITION: Funcion que descarga un fichero con un determinado id del servidor
	PARAMETERS: fileID - Id del fichero que se desea descargar
	RETURN: Contenido del fichero que se ha obtenido del servidor
	"""
	def downloadFile(self, fileID):
		areErrors = 0
		url = self.secureBoxURL + "/api/files/download"
		headers = {"Authorization": self.authorization}
		# En el cuerpo del mensaje POST se manda el identificador del fichero
		args = {"file_id": fileID}

		response = requests.post(url, headers=headers, json=args, verify=False)

		# Se trata de acceder a los campos de la respuesta de error, si consigue acceder, quiere decir que
		# se ha producido un error
		try:
			jsonResponse = json.loads(response.text)
			# Se informa del codigo del error producido, un mensaje explicativo y se sale de la ejecucion
			print("Error code: " + jsonResponse["error_code"])
			print("There is no file with the ID: " + fileID)
			areErrors = 1
		except:
			# Si no consigue acceder a los campos es que la respuesta es directamemte el mensaje cifrado, por
			# lo que informamos del exito
			print("File with ID:  " + fileID + " downloaded correctly.")

		if areErrors == 1:
			sys.exit()

		# Devolvemos el fichero cifrado
		return response.content

	"""
	NAME: deleteFile
	DEFINITION: Funcion que elimina un fichero con un determinado id del servidor
	PARAMETERS: file_id - Id del fichero que se desea borrar
	RETURN: void
	"""
	def deleteFile(self, file_id):
		url = self.secureBoxURL + "/api/files/delete"
		headers = headers = {"Authorization": self.authorization}
		# Hay que pasar como cuerpo del POST el identificador del fichero
		args = {"file_id" : file_id}

		# Se realiza el envio del mensaje HTTP POST
		response = requests.post(url, headers=headers, json=args, verify=False)
		jsonResponse = json.loads(response.text)

		# Se trata de acceder a la id del fichero eliminado, si no se puede acceder (hay una excepcion) es que se ha producido un error
		try:
			# Si se puede obtener la id del fichero es que ha sido eliminado correctamente, informamos del exito de la operacion y salimos
			id_fichero_eliminado = jsonResponse["file_id"]
			print("The file with ID: " + id_fichero_eliminado + " have been deleted.")
		except:
			# Si no se ha podido obtener la ID del fichero, quiere decir que ha habido un error, informamos del codigo de error y de la
			# descripcion del error
			print()
			print("Error code: " + jsonResponse["error_code"])
			print(jsonResponse["description"])