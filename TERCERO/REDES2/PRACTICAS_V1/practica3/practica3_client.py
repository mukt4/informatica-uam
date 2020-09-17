# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
# import the library
import numpy as np
import cv2, threading
import sys, os, time, socket

from prot_conexion import Prot_conexion
from prot_transporte import Prot_transporte
from appJar import gui
from PIL import Image, ImageTk

"""
CLASS_NAME: VideoClient
Clase encargada de gestionar la parte grafica de la aplicacion
"""
class VideoClient(object):
	"""
	NAME: __init__(constructor)
	DEFNITION: Constructor de la clase VideoClient
	PARAMETERS: window_size - Tamano de la ventana principal
	RETURN: Instancia de la clase VideoClient
	"""
	def __init__(self, window_size):
		# Creamos una variable que contenga el GUI principal
		self.app = gui("Redes2 - P2P", window_size)
		self.app.setGuiPadding(10,10)

		# Preparación del interfaz
		self.app.addLabel("title", "Cliente Multimedia P2P - Redes2 ")
		self.app.addImage("video", "imgs/webcam.gif")

		# Registramos la función de captura de video
		# Esta misma función también sirve para enviar un vídeo
		# self.cap = cv2.VideoCapture(0)
		self.cap = cv2.VideoCapture("video.mp4")
		self.app.setPollTime(20)
		self.app.registerEvent(self.capturaVideo)

		# Añadir los botones
		self.app.addButtons(["Conectar", "Colgar", "Pausar/Reanudar", "Chat", "Terminar", "Salir"], self.buttonsCallback)
		
		# Barra de estado
		# Debe actualizarse con información útil sobre la llamada (duración, FPS, etc...)
		self.app.addStatusbar(fields=2)

		# Anadimos el protocolo de conexion creado
		self.prot_conexion = Prot_conexion()

		# Protocolo de atencion a clientes, gestiona la conexion entre usuarios de modo P2P
		self.prot_transporte = Prot_transporte(self.app, self.prot_conexion.user, self.prot_conexion.port+1, self.cap)

		# Definimos la ventana emergente para la introduccion del usuario a llamar
		self.app.startSubWindow("Usuarios", modal=True)
		self.app.setSize("400x400") # Fijamos el tamano de esta ventana
		diccionarioUsuarios = self.prot_conexion.listUsers() # Obtenemos la lista de usuarios del servidor de descubrimiento
		self.app.addLabel("UsernameLabel", "Seleccione el usuario al que desea llamar")
		self.app.addListBox("Username", diccionarioUsuarios['name']) # Creamos una lista de seleccion con la lista de usuarios
		self.app.addButton("LLAMAR", self.getEntries) # Si seleccionamos a Llamar se comienza la llamada
		self.app.stopSubWindow() # Terminamos de definir la ventana emergente

		# Subwindow para chat
		self.app.startSubWindow("UsuariosChat", modal=True)
		self.app.setSize("400x400") # Fijamos el tamano de esta ventana
		diccionarioUsuarios = self.prot_conexion.listUsers() # Obtenemos la lista de usuarios del servidor de descubrimiento
		self.app.addLabel("UsernameLabelChat", "Seleccione el usuario con el que desea chatear")
		self.app.addListBox("UsernameChat", diccionarioUsuarios['name']) # Creamos una lista de seleccion con la lista de usuarios
		self.app.addButton("CHATEAR", self.getEntriesChat) # Si seleccionamos a Llamar se comienza la llamada
		self.app.stopSubWindow() # Terminamos de definir la ventana emergente

	"""
	NAME: getEntries
	DEFNITION: Funcion que se lanza cuando se pulsa el boton llamar
	PARAMETERS: btn - Boton que ha disparado la ejecucion de esta funcion
	RETURN: void
	"""
	def getEntries(self, btn):
		# Obtenemos de la ventana de seleccion de nombre el nombre seleccionado
		nick = self.app.getListBox("Username")
		nick = nick[0] # Extraemos el elemento de la lista
		# Obtenemos los datos del usuario del servidor de descubrimiento
		busqueda, datos = self.prot_conexion.getUserData(nick)
		# Se informa de un error en caso de no encontrar el usuario
		if busqueda == 'NOK USER_UNKNOWN':
			self.app.errorBox('Error', 'El usuario ' + nick + ' no existe en el sistema')
		else:
			# Si se encuentra el usuario
			# Cerramos la ventana de introduccion de datos
			self.app.hideSubWindow("Usuarios")
			# Creamos un hilo que comience la llamada
			t = threading.Thread(target=self.prot_transporte.comenzarLlamada, args=(datos[1], datos[2], nick))
			t.start()

	def getEntriesChat(self, btn):
		# Obtenemos de la ventana de seleccion de nombre el nombre seleccionado
		nick = self.app.getListBox("UsernameChat")
		nick = nick[0] # Extraemos el elemento de la lista
		# Obtenemos los datos del usuario del servidor de descubrimiento
		busqueda, datos = self.prot_conexion.getUserData(nick)
		# Se informa de un error en caso de no encontrar el usuario
		if busqueda == 'NOK USER_UNKNOWN':
			self.app.errorBox('Error', 'El usuario ' + nick + ' no existe en el sistema')
		else:
			# Si se encuentra el usuario
			# Cerramos la ventana de introduccion de datos
			self.app.hideSubWindow("UsuariosChat")
			# Creamos un hilo que comience la llamada
			t = threading.Thread(target=self.prot_transporte.comenzarChat, args=(datos[1], datos[2], nick))
			t.start()

	"""
	NAME: start
	DEFNITION: Funcion encargada de iniciar la interfaz grafica
	PARAMETERS: None
	RETURN: void
	"""
	def start(self):
		self.app.go()
	
	"""
	NAME: capturaVideo
	DEFNITION: Función que captura el frame a mostrar en cada momento
	PARAMETERS: None
	RETURN: void
	"""
	def capturaVideo(self):
		# Capturamos un frame de la cámara o del vídeo
		ret, frame = self.cap.read()
		frame = cv2.resize(frame, (640,480))
		cv2_im = cv2.cvtColor(frame,cv2.COLOR_BGR2RGB)
		img_tk = ImageTk.PhotoImage(Image.fromarray(cv2_im))		    

		self.prot_transporte.sem.acquire()
		if self.prot_transporte.llamadaEstablecida == 0:
			self.prot_transporte.sem.release()
			# Lo mostramos en el GUI
			self.app.setImageData("video", img_tk, fmt = 'PhotoImage')
		else:
			self.prot_transporte.sem.release()
	
	"""
	NAME: setImageResolution
	DEFNITION: Establece la resolución de la imagen capturada
	PARAMETERS: resolution - Resolucion de la imagen LOW, MEDIUM o HIGH
	RETURN: void
	"""
	def setImageResolution(self, resolution):		
		# Se establece la resolución de captura de la webcam
		# Puede añadirse algún valor superior si la cámara lo permite
		# pero no modificar estos
		if resolution == "LOW":
			self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, 160) 
			self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 120) 
		elif resolution == "MEDIUM":
			self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, 320) 
			self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 240) 
		elif resolution == "HIGH":
			self.cap.set(cv2.CAP_PROP_FRAME_WIDTH, 640) 
			self.cap.set(cv2.CAP_PROP_FRAME_HEIGHT, 480) 
	
	"""
	NAME: buttonsCallback
	DEFNITION: Función que gestiona los callbacks de los botones
	PARAMETERS: button - Descripcion del buton que ha disparado la ejecucion de esta funcion
	RETURN: void
	"""
	def buttonsCallback(self, button):
		# Si hemos seleccionado salir
		if button == "Salir":
			self.prot_transporte.sem.acquire()
			if self.prot_transporte.llamadaEstablecida != 0: # Comprobamos que no hay una llamada establecida
				self.prot_transporte.sem.release()
				self.prot_transporte.app.errorBox('Error', 'Debes colgar la llamada actual para poder salir de la aplicacion')
				return
			self.prot_transporte.sem.release() # Liberamos el uso de la variable global llamadaEstablecida

			# Nos conectamos al socket para TCP de control de nuestro porpio proceso
			# (hilo atenderLlamadas) para desbloquearlo del accept() y enviamos EXIT para que termine.
			sockSalir = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			server_address = ('', self.prot_conexion.port)
			sockSalir.connect(server_address)
			sockSalir.send('EXIT'.encode())
			sockSalir.close()

			# Cerramos los sockets que aún están abiertos
			self.prot_conexion.sock.close()
			self.prot_transporte.sockControl.close()
			self.prot_transporte.sockRecibir.close()
	    	# Salimos de la aplicación
			self.app.stop()
		# Si hemos seleccionado conectar
		elif button == "Conectar":
			# Comprobamos que no tenemos ya una llamada establecida (en cuyo caso informamos al usuario)
			self.prot_transporte.sem.acquire()
			if self.prot_transporte.llamadaEstablecida != 0:
				self.prot_transporte.sem.release()
				self.prot_transporte.app.errorBox('Error', 'Debes colgar la llamada actual para poder realizar otra')
				return
			self.prot_transporte.sem.release()
			# Mostramos la ventana para introducir el usuario con el que queremos conectar
			self.app.showSubWindow("Usuarios") 
		# Si hemos seleccionado colgar
		elif button == 'Colgar':
			# Ponemos el estado del video a 2 (lo colgamos).
			# Simplemente hay que hacer esto ya que hay funciones pendientes de este cambio de estado
			self.prot_transporte.semVideo.acquire()
			self.prot_transporte.estadoVideo = 2
			self.prot_transporte.semVideo.release()   
		# Si hemos seleccionado pausar o reanudar
		elif button == "Pausar/Reanudar":
			# Comprobamos si hay una llamada establecida (si no hay este boton no tiene funcion)
			self.prot_transporte.sem.acquire()
			if self.prot_transporte.llamadaEstablecida == 0:
				self.prot_transporte.sem.release()
				return
			self.prot_transporte.sem.release()

			# Ponemos el estado del video a 1 (lo pausamos) si el estado del video es 0 (en reproduccion)
			self.prot_transporte.semVideo.acquire()
			if self.prot_transporte.estadoVideo == 0:
				self.prot_transporte.estadoVideo = 1
				self.prot_transporte.semVideo.release()
			# Ponemos el estado del video a 0 (reanudamos) si el estado del video es 1 (parado)
			elif self.prot_transporte.estadoVideo == 1:
				self.prot_transporte.estadoVideo = 0
				self.prot_transporte.semVideo.release()
			# En cualquier otro caso hay que liberar la variable estadoVideo soltando el semaforo
			else:
				self.prot_transporte.semVideo.release()
		elif button == "Chat":
			self.prot_transporte.sem.acquire()
			if self.prot_transporte.llamadaEstablecida != 0:
				self.prot_transporte.sem.release()
				self.prot_transporte.app.errorBox('Error', 'Debes colgar la llamada actual para poder realizar otra')
				return
			self.prot_transporte.sem.release()
			# Mostramos la ventana para introducir el usuario con el que queremos conectar
			self.app.showSubWindow("UsuariosChat") 
		elif button == "Terminar":
			# Ponemos el estado del video a 3, es decir cerramos el chat
			self.prot_transporte.semVideo.acquire()
			self.prot_transporte.estadoVideo = 3
			self.prot_transporte.semVideo.release()  



if __name__ == '__main__':

	vc = VideoClient("640x520")

	# Crear aquí los threads de lectura, de recepción y,
	# en general, todo el código de inicialización que sea necesario
	# ...


	# Lanza el bucle principal del GUI
	# El control ya NO vuelve de esta función, por lo que todas las
	# acciones deberán ser gestionadas desde callbacks y threads
	vc.start()