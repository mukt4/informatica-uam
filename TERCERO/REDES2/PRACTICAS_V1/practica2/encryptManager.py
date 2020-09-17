# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
from Crypto.PublicKey import RSA
from Crypto.Hash import SHA256
from Crypto.Cipher import AES, PKCS1_OAEP
from Crypto.Random import get_random_bytes
from Crypto.Util.Padding import pad, unpad
from Crypto.Signature import pkcs1_15

"""
CLASS_NAME: EncryptManager
Esta clase se encargara de gestionar todo lo referido a cifrados, firmas y generacion de claves
"""
class EncryptManager:
	RSALength = 2048 # Longitud de la clave RSA 
	AESLength = 32   # Longitud de clave AES
	IVLength = 16 # El vector de inicializacion debe tener 16 bytes
	BlockSize = 32 # El tamano de bloque CBC es de 32 bytes

	"""
	NAME: createKeys
	DEFINITION: Funcion encargada de la generacion de claves publica y privada
	PARAMETERS: None
	RETURN: Para de claves publica/privada
	"""
	def createKeys(self):
		key = RSA.generate(self.RSALength)
		return key

	"""
	NAME: crearFirma
	DEFINITION: Funcion que dado un fichero crea su firma digital
	PARAMETERS: file - Nombre del fichero que se desea firmar
				key - Clave privada con la que se firmara
	RETURN: Firma del documento
	"""
	def crearFirma(self, file, key):
		# Abrimos el fichero del que vamos a generar la clave
		try:
			fichero = open(file, "rb")
		except IOError:
			print("File '" + file + "' does not exist")
			exit()

		# Leemos de forma binaria el contenido del fichero a mandar
		# Vamos a calcular el hash sobre este contenido
		contenido = fichero.read()

		# Obtenemos la clave privada, pues vamos a cifrar usando RSA el hash
		privateKey = RSA.import_key(key)

		# Calculamos en content hash el hash del fichero a mandar
		objHash = SHA256.new(data=contenido)

		# Ciframos el hash del documento con nuestra clave privada y obtenemos la firma
		firma = pkcs1_15.new(privateKey).sign(objHash)

		fichero.close()
		return firma

	"""
	NAME: encriptar 
	DEFINITION: Funcion que encripta un fichero con la clave publica dada
	PARAMETERS: file - Nombre del fichero que se desea cifrar
				publicKey - Clave publica con la que se cifrara el fichero
	RETURN: Documento cifrado
	"""
	def encriptar(self, file, publicKey):
		# Abrimos el fichero del que vamos a generar la clave
		try:
			fichero = open(file, "rb")
		except IOError:
			print("File '" + file + "' does not exist")
			exit()

		# Leemos el mensaje a cifrar
		contenido = fichero.read()
		fichero.close()

		# Importamos la clave publica del receptor
		publicKeyReceptor = RSA.import_key(publicKey)

		# Ciframos el mensaje con RSA y la clave publica del receptor
		cipher_rsa = PKCS1_OAEP.new(publicKeyReceptor)
		mensajeCifrado = cipher_rsa.encrypt(contenido)

		return mensajeCifrado

	"""
	NAME: generar_mensaje_cifrado
	DEFINITION: Funcion que cifra un mensaje incluyendo al principio la firma mediante AES, 
				y luego posteriormente firma la clave AES con la clave publica RSA del receptor.
	PARAMETERS: file - fichero que se firmara y cifrara
				firma - firma que se concatenara al fichero
				publicKey - Clave con la que se cifrara
	RETURN: Fichero firmado y cifrado
	""" 
	def generar_mensaje_cifrado(self, file, firma, publicKey):
		# Abrimos el fichero del que vamos a generar la clave
		try:
			fichero = open(file, "rb")
		except IOError:
			print("File '" + file + "' does not exist")
			exit()

		# Leemos el mensaje a cifrar
		contenido = fichero.read()

		# Le incluimos al principio la firma digital ya generada
		mensajeACifrar = firmqa + contenido

		# Obtenemos la clave publica del receptor
		publicKeyReceptor = RSA.import_key(publicKey)

		# Vamos a generar la clave simetrica AES para cifrar el mensaje
		clave_simetrica = get_random_bytes(EncryptManager.AESLength)

		# Vamos a generar el vector de inicializacion, que tiene que tener 16 bytes
		init_vector = get_random_bytes(EncryptManager.IVLength)

		cifrador = AES.new(clave_simetrica, AES.MODE_CBC, iv=init_vector)

		# Para poder cifrar el mensaje debemos ajustar la longitud del mensaje al tamaño
		# del bloque usado (en este caso de 32 bytes)
		padded_data = pad(mensajeACifrar, EncryptManager.BlockSize)

		try:
			# Se encripta el mensaje (Firma + Contenido)
			textoCifrado = cifrador.encrypt(padded_data)
			print("Symmetric encryption of sign and message: OK")
		except:
			print("Symmetric encryption of sign and message: ERROR")
			exit()

		# Ahora que hemos generado el mensaje cifrado, vamos a cifrar la
		# clave simetrica con la clave publica del receptor para generar
		# el sobre digital

		# Ciframos la clave simetrica (AES) con la clave publica (RSA) del receptor
		cipher_rsa = PKCS1_OAEP.new(publicKeyReceptor)

		try:
			claveCifrada = cipher_rsa.encrypt(clave_simetrica)
			print("Asymmetric encryption of the symmetric key: OK")
		except:
			print("Asymmetric encryption of the symmetric key: ERROR")
			exit()

		# Componemos el mensaje final como clave AES cifrada con la publica del destinatario mas 
		# el mensaje con la firma cifrado
		# Es necesario añadir el IV para poder desencriptarlo con AES
		return init_vector + claveCifrada + textoCifrado

	"""
	NAME: descifrarMensaje
	DEFINITION: Funcion que descifra el sobre digital con la clave privada del receptor para obtener 
				la clave de sesion AES, con dicha clave descifra el mensaje.
	PARAMETERS: fichero - Fichero donde se guardara el mensaje descifrado
				contenido - Mensaje que se desea cifrar
				clavePublica - Clave con la que se cifro el mensaje
				clavePrivada - Clave con la que descifraremos el mensaje
	RETURN: 0 si no se ha producido ningun error
			-1 si se ha producido algun error
	"""
	def descifrar_mensaje(self, fichero, contenido, clavePublica, clavePrivada):
		# Obtenemos la clave privada
		privateKey = RSA.import_key(clavePrivada)
		# Obtenemos la clave publica para comprobar la firma
		publicKey = RSA.import_key(clavePublica)
		#Generamos un cifrador para obtener la clave AES
		cipher_rsa = PKCS1_OAEP.new(privateKey)

		try:
			#Desencriptamos con nuestra clave privada la clave AES con la que esta cifrado el la firma y el mensaje
			claveAES = cipher_rsa.decrypt(contenido[16:272])
			print("Obtaining symmetric key: OK")
		except:
			print("Obtaining symmetric key: ERROR")
			exit()
		# Creamos un cifrador con el IV (16 bytes) y la clave que hemos desencriptado
		cipher_aes = AES.new(claveAES, AES.MODE_CBC, iv=contenido[:16])
		try:
			# Obtenemos los datos junto con la firma
			datosFirmados = cipher_aes.decrypt(contenido[272:])
			print("Deciphering message with symmetric key: OK")
		except:
			print("Deciphering message with symmetric key: ERROR")
			exit()

		# Eliminamos el posible padding que haya en el mensaje
		unpadded_datosFirmados = unpad(datosFirmados, EncryptManager.BlockSize)

		# Generamos el hash de los datos en claro para comprobar que el fichero es autentico
		objHash = SHA256.new(data=unpadded_datosFirmados[256:])

		# Verificamos la firma descencriptandola con la la clave privada
		verifier = pkcs1_15.new(publicKey)
		
		try:
			# Comprobamos que el hash calculado del mensaje coincide con el hash desencriptado del emisor
			verifier.verify(objHash, unpadded_datosFirmados[:256])
			# Como el mensaje esta autenticado y descifrado, lo guardamos en un fichero con el contenido
			file = open("decrypt_" + fichero, "wb")
			file.write(unpadded_datosFirmados[256:])
			file.close()
			# Devolvemos 0 para indicar que el mensaje ha sido desencriptado y autenticado
			return 0
		except (ValueError, TypeError):
			# El mensaje no se ha podido autenticar
			return -1
	