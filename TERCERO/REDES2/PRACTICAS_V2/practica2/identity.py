# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import _pickle as pickle
from Crypto.PublicKey import RSA
from Crypto.Cipher import AES
import os.path
import hashlib
import os

"""
CLASS_NAME: Identity
Esta clase se encargara de manejar todo lo referido a la gestion de identidades
"""
class Identity:
	IV_SIZE = 16
	KEY_SIZE = 32
	SALT_SIZE = 16

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

	"""
	NAME: loadIdentity
	DEFINITION: Funcion que carga una identidad de la clase desde un fichero
	PARAMETERS: fileName - Nombre del fichero desde el que se cargara la identidad
	RETURN: Instancia de identity cargada
	"""
	def loadIdentity(fileName):
		IV_SIZE = 16
		KEY_SIZE = 32
		SALT_SIZE = 16

		# Si ya se ha descifrado el fichero de usuario
		if os.path.isfile('decrypt.bin'):
			with open('decrypt.bin', 'rb') as file:
				unpickler = pickle.Unpickler(file)
				return unpickler.load()
		else:
			# Se abre el fichero en modo lectura binaria
			with open(fileName, "rb") as file:
				encrypted = file.read()
				salt = encrypted[0:SALT_SIZE]
				password = input('Enter your password: ')
				derived = hashlib.pbkdf2_hmac('sha256', password.encode(), salt, 100000,
				                              dklen=IV_SIZE + KEY_SIZE)
				iv = derived[0:IV_SIZE]
				key = derived[IV_SIZE:]
				data = AES.new(key, AES.MODE_CFB, iv).decrypt(encrypted[SALT_SIZE:])
				file.close()
				with open('decrypt.bin', "wb") as file:
					file.write(data)
					file.close()
					with open('decrypt.bin', 'rb') as file:
						# Usamos la libraria pickler para leer el objeto de disco
						unpickler = pickle.Unpickler(file)
						try:
							# Devolvemos el Objeto Identity
							objeto = unpickler.load()
							return objeto
						except:
							return None

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
			file.close()
			with open(fileName, "rb") as file:
				data = file.read()
				file.close()
				os.remove(fileName)
				with open(fileName, "wb") as file:
					password = input('Enter your user password: ')
					salt = os.urandom(self.SALT_SIZE)
					derived = hashlib.pbkdf2_hmac('sha256', password.encode(), salt, 100000,
					                              dklen=self.IV_SIZE + self.KEY_SIZE)
					iv = derived[0:self.IV_SIZE]
					key = derived[self.IV_SIZE:]

					encrypted = salt + AES.new(key, AES.MODE_CFB, iv).encrypt(data)
					file.write(encrypted)
					file.close()

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
