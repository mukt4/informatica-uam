# Practica realizada por Tomas Higuera Viso y Guillermo Hoyo Bravo
import socket, os, threading, sys
import numpy as np
import cv2
import fcntl, os
import errno
import ssl

from PIL import Image, ImageTk

"""
CLASS_NAME: Prot_transporte
Clase encargada de gestionar todo el protocolo de llamada entre dos clientes
"""
class Prot_transporte:
	
	#Semaforo encargado de regular el acceso a la variable llamadaEstablecida
	sem = threading.Semaphore()
	#Semaforo encargado de regular el acceso a la variable estadoVideo
	semVideo = threading.Semaphore()
	#Semaforo encargado de regular el acceso a la variable miniVideo
	semMiniVideo = threading.Semaphore()
	#Variable encargada de guardar los frames capturados por la camara durante una llamada para poder mostrarlos en la pantalla
	miniVideo = None
	#Variable que guarda el estado en el que se encuentra la aplicacion. 0 no estamos en una llamada, 1 estamos en una llamada
	llamadaEstablecida = 0
	#Variable que gaurda el estado de la llamada. 0 no estamos en una llamada, 1 estamos en una llamada pero esta detenida
	# y 2 estamos colgando una llamada
	estadoVideo = 0

	"""
	NAME: __init__(constructor)
	DEFINITION: Constructor de la clase Prot_transporte
	PARAMETERS: app - Instancia de la interfaz grafica
				nick - Nombre de usuario que vamos a utilizar
				port - Puerto en el que estamos escuchando
				cap - Instancia del objeto encargado de realizar la captura de video
	RETURN: Instancia de la clase Prot_transporte
	"""
	def __init__(self, app, nick, port, cap):
		#Instancia de la interfaz grafica
		self.app = app
		#Nombre del usuario que utilizamos
		self.nick = nick
		#Puerto en el que recibiremos datos UDP (TCP en port-1)
		self.port = port
		#Capturador de video
		self.cap = cap
		# Ciframos la conexion con ssl
		context = ssl.SSLContext(ssl.PROTOCOL_TLS)
		context.load_cert_chain('keys/rootCA.crt', 'keys/rootCA.key')
		# Socket TCP que esta a la escucha de llamadas
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.sock.bind(('', self.port-1))
		self.sock.listen(10)
		self.sockControl = context.wrap_socket(self.sock, server_side=True)

		# Socket UDP de escucha
		self.sockRecibir = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
		self.sockRecibir.bind(('', self.port))

		# Creamos un hilo que atienda a las llamadas
		self.atenderLlamadas = threading.Thread(target=self.atenderLlamadas)
		self.atenderLlamadas.start()

	"""
	NAME: atencionLlamadasHilo
	DEFINITION: Funcion encargada de gestionar los hilos de envio y recibo de datos
	PARAMETERS: sock - Socket TCP para realizar el control
				addr - Direccion ip y puerto del otro extremo de la  llamada
				dataParsed - Contenido que hemos recibido en el protocolo de inciacion del control de la llamada
	RETURN: void
	"""
	def atencionLlamadasHilo(self, sock, addr, dataParsed):
		# Abrir socket UDP dataParsed[2] de envio
		self.sockEnviar = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
		# Cogenmos la direccion Ip de la conexion TCP y el puerto que nos indica la otra persona
		direccionDestino = (addr[0], int(dataParsed[2]))

		# Se crean los dos hilos de envio y de recepcion de datos de control
		# El argumento que recibe es el socket al que mandar los comandos de control
		hiloEnvioControl = threading.Thread(target=self.enviarComandosControl, args=(sock,))
		hiloEnvioControl.start()

		# Se crea tambien el hilo de recepcion de los datos de control, que se recibe por el mismo socket.
		hiloRecepcionControl = threading.Thread(target=self.recibirComandosControl, args=(sock,))
		hiloRecepcionControl.start()

		# Se crean los dos hilos de envio y recibo de datos (UDP)
		hiloEnviar = threading.Thread(target=self.enviarVideo, args=(self.sockEnviar, direccionDestino))
		hiloRecibir = threading.Thread(target=self.recibirVideo, args=(self.sockRecibir,))
		hiloEnviar.start()
		hiloRecibir.start()

		#Se recogen los hilos cuando se termine la llamada
		hiloEnviar.join()
		hiloRecibir.join()
		hiloEnvioControl.join()
		hiloRecepcionControl.join()
		
		# Se pone la variable como que no hay llamada
		self.sem.acquire()
		self.llamadaEstablecida = 0
		self.sem.release()
		# Se reinicia la variable estadoVideo a 0
		self.semVideo.acquire()
		self.estadoVideo = 0
		self.semVideo.release()

		# Cerramos el socket que hemos creado
		self.sockEnviar.close()
		sock.close()
		sys.exit()

	"""
	NAME: atenderLlamadas
	DEFINITION: Funcion que escucha en el puerto y esta pendiente de atender llamada y gestionar el protocolo
				incial
	PARAMETERS: None
	RETURN: void
	"""
	def atenderLlamadas(self):
		while True:
			#Se bloquea a la espera de clientes
			sock, addr = self.sockControl.accept()

			#Lee el mensaje que le envia el cliente
			dataReaded = sock.recv(1024)

			#Parseo de los datos separando por espacios
			dataParsed = dataReaded.decode().split()
			#Si el messaje es diferente de CALLING puede ser que se cierre la aplicacion o que se trate de un error
			if dataParsed[0] != 'CALLING':
				#En caso de recibir un EXIT quiere decir que proviene de la propia aplicacion y que el hilo tiene que terminar
				#Se utiliza esto para desbloquear el hilo del accept()
				if dataParsed[0] == 'EXIT':
					sys.exit()
				else:
					#Sino, es un mensaje de error y hay que denegar la llamada
					# La palabra no es CALLING asique se deniega la llamada
					sock.send(('CALL_DENIED ' + self.nick).encode())
					sock.close()
			else:
				self.sem.acquire()
				if self.llamadaEstablecida == 1:
					# liberamos el semaforo
					self.sem.release()
					# En caso de recibir una llamada y estamos actualmente en otra esta se rechaza
					# Enviamos el mensaje de ocupado
					sock.send(('CALL_DENIED ' + self.nick).encode())
					sock.close()
				else:
					# No estamos en una llamada y vamos a proceder a atenderla
					# liberamos el semaforo
					self.llamadaEstablecida = 1
					self.sem.release()
					answer = self.app.yesNoBox('Llamada entrante', '¿Quieres aceptar la llamada de ' + dataParsed[1] + '?')
					# Aceptmos la llamada entrante
					if answer == True:
						sock.send(('CALL_ACCEPTED ' + self.nick + ' ' + str(self.port)).encode())

						# Creamos el hilo que va a gestionar la llamada, los sockets se cierran ahi
						hiloGestionLlamadas = threading.Thread(target=self.atencionLlamadasHilo, args=(sock, addr, dataParsed))
						hiloGestionLlamadas.start()
					#Denegamos la llamada
					else:
						#Se establece el estado de la aplicacion a no llamada
						self.sem.acquire()
						self.llamadaEstablecida = 0
						self.sem.release()
						# Enviamos el mensaje de ocupado
						sock.send(('CALL_DENIED ' + self.nick).encode())
						sock.close()

	"""
	NAME: comenzarLlamada
	DEFINITION: Funcion encargada de comenzar una llamada con un usuario
	PARAMETERS: ip - Ip del otro extremo de la llamada
				puerto - Puerto de escucha del otro extremo de la llamada
				nick - Nick del usuario al que se esta llamando
	RETURN: void
	"""	
	def comenzarLlamada(self, ip, puerto, nick):
		self.sem.acquire()
		if self.llamadaEstablecida == 1:
			# liberamos el semaforo
			self.sem.release()
			#Error mostrado en caso de estar en una llamada
			self.app.errorBox('Error', 'Debes colgar la llamada actual')
			return
		# liberamos el semaforo modificamos la variable de estado
		self.llamadaEstablecida = 1
		self.sem.release()
		#Creamos un socket para realizar la conexion de control Full Duplex
		self.sockControlDestino = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		server_address = (ip, int(puerto))
		# Nos conectamos con ssl
		context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
		context.load_verify_locations('keys/client.crt')
		#Intentamos conectarnos al otro usuario
		try:
			self.sockControlDestino.connect(server_address)
			context.wrap_socket(sock, server_hostname=ip)
		except:
			# Se produce una excepcion en caso de que el otro socket no este escuchando
			self.app.errorBox('Error', 'El usuario ' + nick + ' no esta conectado en este momento')
			#Se vuelve a poner el estado a no llamada
			self.sem.acquire()
			self.llamadaEstablecida = 0
			self.sem.release()
			sys.exit()


		# Envio CALLING y el puerto UDP de escucha
		self.sockControlDestino.send(('CALLING ' + nick + ' ' + str(puerto)).encode())

		# Se recibe la respuesta de la otra persona
		mensajeRecibido = self.sockControlDestino.recv(1024)

		# Se parsea la llamada
		parsedMensajeRecibido = mensajeRecibido.decode().split()
		#Si el cliente ha aceptado la llamada
		if parsedMensajeRecibido[0] == 'CALL_ACCEPTED':
			# Abrir socket UDP dataParsed[2] de envio
			self.sockEnviar = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
			# Cogenmos la direccion Ip de la conexion TCP y el puerto que nos indica la otra persona
			direccionDestino = (ip, int(parsedMensajeRecibido[2]))

			# Se crean los dos hilos de envio y de recepcion de datos de control
			# El argumento que recibe es el socket al que mandar los comandos de control
			hiloEnvioControl = threading.Thread(target=self.enviarComandosControl, args=(self.sockControlDestino,))
			hiloEnvioControl.start()

			# Se crea tambien el hilo de recepcion de los datos de control, que se recibe por el mismo socket.
			hiloRecepcionControl = threading.Thread(target=self.recibirComandosControl, args=(self.sockControlDestino,))
			hiloRecepcionControl.start()

			# Se crean los dos hilos de envio y recibo de datos
			hiloEnviar = threading.Thread(target=self.enviarVideo, args=(self.sockEnviar, direccionDestino))
			hiloRecibir = threading.Thread(target=self.recibirVideo, args=(self.sockRecibir,))
			hiloEnviar.start()
			hiloRecibir.start()

			#Se recogen todos los hilos creados
			hiloEnviar.join()
			hiloRecibir.join()
			hiloEnvioControl.join()
			hiloRecepcionControl.join()

			# Se pone la variable como que no hay llamada
			self.sem.acquire()
			self.llamadaEstablecida = 0
			self.sem.release()
			# Se reinicia el estado de la llamada
			self.semVideo.acquire()
			self.estadoVideo = 0
			self.semVideo.release()

			# Se cierran los socket usados en la llamada
			self.sockControlDestino.close()
			self.sockEnviar.close()
		else:
			# El otro usuario ha rechazado la llamada
			self.sem.acquire()
			self.llamadaEstablecida = 0
			self.sem.release()
			#Mensaje de error
			self.app.errorBox('Error', parsedMensajeRecibido[1] + ' no esta disponible en este momento')
			self.sockControlDestino.close()
			return

	"""
	NAME: enviarVideo
	DEFINITION: Funcion encargada de enviar los frames de video al otro usuario
	PARAMETERS: sockEnviar - Socket por el que se envian los datos 
				direccionDestino - Direccion ip y puerto del otro usuario
	RETURN: void
	"""
	def enviarVideo(self, sockEnviar, direccionDestino):
		# 0 significa estamos en una llamada
		# 1 significa que esta pausado
		# Otro valor que hemos colgado
		while True:
			self.semVideo.acquire()
			#En caso de que el video no este en proceso de colgarse
			if self.estadoVideo != 2:
				self.semVideo.release()
				# Obtener un frame para enviar
				ret, frame = self.cap.read()
				# Se genera una imagen de 75% de compresion
				encode_param = [cv2.IMWRITE_JPEG_QUALITY,75]
				# Se comprime la imagen en formato jpg
				result,encimg = cv2.imencode('.jpg',frame,encode_param)
				if result == False: print('Error al codificar imagen')

				# Cambiar tamaño de la imagen local
				self.semMiniVideo.acquire()
				self.miniVideo = cv2.resize(frame, (160,120))
				self.semMiniVideo.release()

				self.semVideo.acquire()
				# Si el video no esta detenido se envia el frame
				if self.estadoVideo == 0:
					self.semVideo.release()
					sockEnviar.sendto(encimg.tobytes(), direccionDestino)
				self.semVideo.release()
			# Si el video esta en proceso de colgarse, se cierra el socket y se termina el hilo
			elif self.estadoVideo == 2:
				self.semVideo.release()
				# Cerramos el socket
				sockEnviar.close()
				# Se cierra el hilo
				sys.exit()

	"""
	NAME: recibirVideo
	DEFINITION: Funcion encargada de recibir los frames de video del otro usuario
	PARAMETERS: sockRecibir: Socket en el que se reciben los frames del video 
	RETURN: void
	"""
	def recibirVideo(self, sockRecibir):
		# 0 significa estamos en una llamada
		# 1 significa que esta pausado
		# Otro valor que hemos colgado
		while True:
			self.semVideo.acquire()
			if self.estadoVideo != 2:
				self.semVideo.release()
				# Obtener un frame para enviar
				# Recibir un frame

				self.semVideo.acquire()
				# Si el video no esta detenido se recibe el video
				if self.estadoVideo == 0:
					self.semVideo.release()
					frame, addr = sockRecibir.recvfrom(65536)
					decimg = cv2.imdecode(np.frombuffer(frame, np.uint8),1)
				else:
					self.semVideo.release()

				# Combinar imagenes local + recibida
				self.semMiniVideo.acquire()
				#Si la variable donde se guarda la imagen local no es None
				if self.miniVideo is not None:
					# Combinar imagenes
					decimg[:120, :160] = self.miniVideo
				self.semMiniVideo.release()

				# Se codifica la imagen adecuadamente para ser mostrada en la aplicacion
				cv2_im = cv2.cvtColor(decimg,cv2.COLOR_BGR2RGB)
				img_tk = ImageTk.PhotoImage(Image.fromarray(cv2_im))

				# Lo mostramos en el GUI
				self.app.setImageData("video", img_tk, fmt = 'PhotoImage')

			elif self.estadoVideo == 2:
				self.semVideo.release()
				# Este socket no hay que cerrarlo
				# Se cierra el hilo
				sys.exit()

	"""
	NAME: enviarComandosControl
	DEFINITION: Funcion encargada de enviar los comandos de control de la llamada
	PARAMETERS: sockControlDestino - Socket de control de la llamada
	RETURN: void
	"""
	def enviarComandosControl(self, sockControlDestino):
		# El estado del video siempre comienza a cero (en reproduccion)
		estadoVideoAnterior = 0
		while True:
			self.semVideo.acquire()
			if estadoVideoAnterior != self.estadoVideo: # El estado del video anterior es distinto que el estado del video actual (se ha hecho alguna accion)
				estadoVideoAnterior = self.estadoVideo # Se actualiza el valor de estadoVideoAnterior
				self.semVideo.release() # Soltamos la variable global, ya tenemos su valor en estadoVideoAnterior
				if estadoVideoAnterior == 0:
					# Se ha reanudado la llamada, lo notificamos asi al otro lado
					sockControlDestino.send(('CALL_RESUME').encode())
				elif estadoVideoAnterior == 1:
					# Se ha pausado la llamada, lo notificamos asi al otro lado
					sockControlDestino.send(('CALL_HOLD').encode())
				else:
					# Se ha cortado la llamada, lo notificamos asi al otro lado y cerramos el hilo
					sockControlDestino.send(('CALL_END').encode())
					sys.exit()
			else:
				# Si son iguales no se manda nada, pero hay que liberar el semaforo
				self.semVideo.release()

	"""
	NAME: recibirComandosControl
	DEFINITION: Funcion encargada de recibir los comandos de control de la llamada
	PARAMETERS: sockControlDestino - Socket de control de la llamada
	RETURN: void
	"""
	def recibirComandosControl(self, sockControlDestino):
		while True:
			dataReaded = sockControlDestino.recv(1024) # Recibimos un mensaje del otro usuario
			dataReaded = dataReaded.decode().split()
			if dataReaded:
				# Le asignamos el valor recibido con su valor correspondiente
				if dataReaded[0] == 'CALL_RESUME':
					valueReaded = 0
				elif dataReaded[0] == 'CALL_HOLD':
					valueReaded = 1
				elif dataReaded[0] == 'CALL_END':
					valueReaded = 2
				else:
					valueReaded = -1

				# Ahora tenemos el valor recibido por la red, vamos a compararlo con el anterior que tenemos
				# si es igual no tenemos que hacer nada
				self.semVideo.acquire()
				if self.estadoVideo != valueReaded:
					# Si son distintos lo actualizamos
					self.estadoVideo = valueReaded
					self.semVideo.release()
				else:
					self.semVideo.release() # Si son iguales no tenemos que actualizar nada pero debemos liberar la variable global

				# Si value readed es 2 se ha colgado la llamada asi que se termina el hilo de recepcion de comandos
				if valueReaded == 2:
					sys.exit()