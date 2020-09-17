# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import socket
import os
import re
import _pickle as pickle
import time
import ssl

from urllib.request import urlopen

"""
CLASS_NAME: Prot_conexion
Clase encargada de gestionar la conexion con el servidor de descubrimiento. Busca los usuarios
disponibles y los lista
"""
class Prot_conexion:
	"""
	NAME: __init__(constructor)
	DEFINITION: Constructor del objeto Prot_conexion
	PARAMETERS: None
	RETURN: Instancia de la clase Prot_conexion
	"""
	def __init__(self):
		# Creacion de un socket para la comunicacion con el servidor de descubrimiento
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		host_ip = socket.gethostbyname('vega.ii.uam.es')
		server_address = (host_ip, 8000) # Especificamos la direccion del servidor
		self.sock.connect(server_address)
		# Datos del usuario, se indican los datos con los que nos vamos a registrar en el servidor
		# de descubrimiento
		# self.user = "Goyo0"
		self.user = 'Goyo1'


		# Obtener ip automaticamente
		# self.ip = urlopen('http://ip.42.pl/raw').read().decode()
		self.ip = "localhost"

		# Se especifica el puerto que vamos a reservar para la conexion de control (tambien es necesario
		# reservar el puerto indmediatamente superior para la conexion UDP)
		# Puerto de control
		# self.port = 4324
		# self.port = 4326
		self.port = 4328

		# Especificamos la contrasena del usuario para el registro
		self.password = 'huevofrito' 

		# Se mandan los datos al servidor para registrarlos
		cadenaRegistro = 'REGISTER ' + self.user + ' ' + self.ip + ' ' + str(self.port) + ' ' + self.password + ' V1#V2\r\n'
		self.sock.send(cadenaRegistro.encode())
		readed = self.sock.recv(1024)
		# Comprobar que se ha realizado el registro correctamente
		if(readed == b'NOK WRONG_PASS'):
			print('Contrasegna invalida para el usuario ' + user)
			self.sock.close()
			exit()

	"""
	NAME: listUsers
	DEFINITION: Metodo que se encarga de listar los usuarios que hay en el servidor
	PARAMETERS: None
	RETURN: Diccionario que contiene todos los usuarios proporcionados por el servidor de descubrimiento
	"""
	def listUsers(self):
		dataReaded = ''
		# Enviar el comando para listar usuarios
		self.sock.send('LIST_USERS'.encode())
		# Leer los datos devuelto
		while True:
			time.sleep(0.25)
			readed = self.sock.recv(1024)
			dataReaded += readed.decode()
			if len(readed) < 1024:
				break

		palabras = re.split('#| ', dataReaded) # Partimos los datos leidos por los distintos campos
		i = 0
		# Inicializamos el diccionario
		dictUsers = {}
		dictUsers['name'] = [] # Lista con los nombres de todos los usuarios
		dictUsers['ip'] = [] # Lista con las direcciones IP de todos los usuarios
		dictUsers['port'] = [] # Lista con el puerto de control de escucha de todos los usuarios
		dictUsers['ts'] = [] # Lista con el timestamp de registro de todos los usuarios
		# Incluimos las palabras que vamos obteniendo en sus respectivos campos
		for palabra in palabras[3:]:
			mod = i%4
			if mod == 0:
				dictUsers['name'].append(palabra)
			if mod == 1:
				dictUsers['ip'].append(palabra)
			if mod == 2:
				dictUsers['port'].append(palabra)
			if mod == 3:
				dictUsers['ts'].append(palabra)
			i = i+1
		# Incluimos un quinto campo en el diccionario con la cantidad de usuarios
		dictUsers['len'] = int(palabras[2])

		return dictUsers

	"""
	NAME: getUserData
	DEFINITION: Metodo que obtiene los datos de un usuario dado su nombre
	PARAMETERS: userName - Nombre del usuario que se va a buscar
	RETURN: Codigo del resultado y datos del usuario 
	"""
	def getUserData(self, userName):
		query = 'QUERY ' + userName + '\r\n'
		# Enviar el comando para obtener los datos del usuario
		self.sock.send(query.encode())
		# Recibimos los datos del usuario
		userDataReaded = self.sock.recv(1024)
		# Dividimos la cadena con los datos en los distintos campos
		palabras = userDataReaded.decode().split()
		# Devolvemos el codigo de la busqueda y los datos, ya divididos
		return userDataReaded.decode(), palabras[2:]
