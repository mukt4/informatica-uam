# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import _pickle as pickle
from Crypto.PublicKey import RSA
from random import randint
from mail_authentication import Email
import os.path

"""
CLASS_NAME: Identity
Esta clase se encargara de manejar todo lo referido a la gestion de identidades
"""
class Identity:
	"""
	NAME: __init__(constructor)
	DEFINITION: Constructor de la clase Identity
	PARAMETERS: name - Nombre del usuario
				email - email del usario
				keys - Par de claves publica/privada
	RETURN: Instancia de la clase Identity creada
	"""
	def __init__(self, name, email, keys):
		self.name = name
		self.email = email
		self.privateKey = keys.exportKey("PEM")
		self.publicKey = keys.publickey().exportKey("PEM")
		self.email_verification = Email()

	"""
	NAME: loadIdentity
	DEFINITION: Funcion que carga una identidad de la clase desde un fichero
	PARAMETERS: fileName - Nombre del fichero desde el que se cargara la identidad
	RETURN: Instancia de identity cargada
	"""
	def loadIdentity(fileName):
		# Se abre el fichero en modo lectura binaria
		with open(fileName, "rb") as file:
			# Usamos la libraria pickler para leer el objeto de disco
			unpickler = pickle.Unpickler(file)
			# Devolvemos el Objeto Identity
			return unpickler.load()

	"""
	NAME: exportIdentity
	DEFINITION: Funcion que exporta la identidad a un fichero
	PARAMETERS: fileName - Nombre del fichero al que se exportara la clase
	RETURN: void
	"""
	def exportIdentity(self, fileName):
		# Se comprueba si existe un fichero ya guardado
		if os.path.isfile("userData.bin"):
			v = ''
			# En caso de que exista se pregunta al usuario si se desea sobreescribir con los datos actuales
			print("New user detected in the system")
			while (v != "Y" and v != "N"):
				v = input("Do you want to create another user? (Y/N): ")
			if v == "N":
				exit()
		# Usando la libreria pickle se guarda el archivo en disco
		with open(fileName, "wb") as file:
			pickle.dump(self, file)

	"""
	NAME: authentication
	DEFINITION: Funcion que manda un correo con un codigo de verificacion para interactuar con el servidor
	PARAMETERS: None
	RETURN: Codigo de verificacion que tendra que introducir el usuario
	"""
	def authentication(self):
		clave = randint(1000, 9999)
		self.email_verification.send_mail(self.email, clave)
		return clave

	"""
	NAME: getPublicKey
	DEFINITION: Getter de la clave publica
	PARAMETERS: None
	RETURN: Public key de la identidad
	"""
	def getPublicKey(self):
		return self.publicKey

	"""
	NAME: getPrivateKey
	DEFINITION: Getter de la clave privada
	PARAMETERS: None
	RETURN: Private key de la identidad
	"""
	def getPrivateKey(self):
		return self.privateKey

	"""
	NAME: getName
	DEFINITION: Getter del nombre
	PARAMETERS: None
	RETURN: Nombre de la identidad
	"""
	def getName(self):
		return self.name

	"""
	NAME: getEmail
	DEFINITION: Getter del email
	PARAMETERS: None
	RETURN: Email de la identidad
	"""
	def getEmail(self):
		return self.email
