# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 2: Extracción de características. Puntos de Interés.
# Parte 3: Similitud correspondencia de puntos de interés.

# librerias y paquetes por defecto
import numpy as np
import scipy
import collections
from skimage.feature import corner_peaks, plot_matches
from p2_utils import PRECISION,cls,test_p2_tarea3
from skimage import color, io
import matplotlib.pyplot as plt
from p2_tarea1 import detectar_puntos_interes_harris
from p2_tarea2 import descripcion_puntos_interes

# Incluya aqui las librerias que necesite en su codigo
# ...

def correspondencias_puntos_interes(descriptores_imagen1, descriptores_imagen2, tipoDist='minDist',max_distancia=25, max_nndr=0.75):
	# Esta funcion determina la correspondencias entre dos conjuntos de descriptores mediante
	# el calculo de la similitud entre los descriptores. 
	#
	# El parametro 'tipoDist' determina el criterio de similitud aplicado 
	# para establecer correspondencias entre pares de descriptores:
	#   - 'mindist': minima distancia euclidea entre descriptores.    
	#
	# La correspondencia indicada por el criterio de similitud se acepta 
	# siempre que la distancia entre descriptores este por encima del umbral 
	# determinado por la variable 'max_distancia'. 
	#     
	# Considere que un descriptor de la imagen 1 puede corresponder a varios de la imagen 2 
	# siempre que se cumplan los criterios anteriores
	#  
	# Argumentos de entrada:
	#   - descriptores1: numpy array de tamaño [numero_descriptores, longitud_descriptor] 
	#                    con los descriptores de los puntos de interes de la imagen 1.        
	#   - descriptores2: numpy array de tamaño [numero_descriptores, longitud_descriptor] 
	#                    con los descriptores de los puntos de interes de la imagen 2.        
	#   - tipoDist: cadena de caracteres que indica el tipo de criterio para establecer correspondencias
	#   - max_distancia: valor de tipo double o float utilizado por el criterio 'minDist', que determina 
	#                    si los descriptores con distancia minima se aceptar como correspondencia
	#
	# Argumentos de salida
	#   - correspondencias: numpy array de tamaño [numero_correspondencias, 2] de tipo int64 
	#                       que determina correspondencias entre descriptores de imagen 1 e imagen 2.
	#                       Por ejemplo: 
	#                       correspondencias[0]=[5,22] significa que el descriptor 5 de la imagen 1 
	#                                                  corresponde con el descriptor 22 de la imagen 2. 
	#                       correspondencias[1]=[5,23] significa que el descriptor 5 de la imagen 1 
	#                                                  corresponde con el descriptor 23 de la imagen 2.
	#
	# NOTA: no modificar los valores por defecto de las variables de entrada tipoDist y max_distancia, 
	#       pues se utilizan para verificar el correcto funciomaniento de esta funcion
	# Realizamos esta asignacion para no tener fallos en los test, todo explicado en la memoria
	correspondencias = np.zeros((0,2))
	# Declaramos esta flag para saber si se ha aniadido algun valor a correspondencias
	flag_vacio = 0
	# Si se trata del tipo distancia minima
	if tipoDist == 'minDist':
		# Hayamos las distancias minimas entre los descriptores de las imagenes
		# Con esta funcion obtendremos una matriz con todas las distancias obtenidas
		distancias = scipy.spatial.distance.cdist(descriptores_imagen1, descriptores_imagen2, metric='euclidean')
		# Seteamos la distancia minima al valor de la maxima distancia para las primeras comparaciones 
		distancia_minima = max_distancia
		# Creamos un array en el que guardaremos los indices despues de asignarlos, para que estos no se repitan
		indices_asignados = []
		# Seteamos una variable flag para saber si hemos encontrado una correspondencia
		flag = 0
		# Realizamos un bucle sobre las distancias calculadas anteriormente
		for i, lista in enumerate(distancias):
			for j, distancia in enumerate(lista):
				# Si el indice no ha sido aniadido pasamos a comprobar el valor de la distancia
				if j not in indices_asignados:
					# Si la distancia es menor que la distancia minima encontrada pasamos a settear los atributos de manera correcta
					if distancia < distancia_minima:
						# Guardamos en una variable auxiliar el indice de la distancia minima encontrada
						aux = j
						# Seteamos la nueva distancia minima
						distancia_minima = distancia
						# Ponemos la flag a 1 para saber que hemos encontrado una correspondencia
						flag = 1
			# Si la flag vale 1, signicara que hemos encotrado una correspondencia
			if flag == 1:
				# Si es la primera correspondencia que se aniade
				if flag_vacio == 0:	
					correspondencias = []
					flag_vacio = 1
				# Aniadimos el indice a la lista de indices asignados
				indices_asignados.append(aux)
				# Guardamos la correspondencia en forma de tupla con los dos indices
				correspondencias.append((i, aux))
				# Volvemos a settear la distancia minima al valor de max distancia para seguir iterando
				distancia_minima = max_distancia
				# Ponemos la flag de correspondencia a 0
				flag = 0 
	# Si el tipo de distancia es nddr
	elif tipoDist == 'nndr':
		# Hayamos las distancias minimas entre los descriptores de las imagenes
		# Con esta funcion obtendremos una matriz con todas las distancias obtenidas
		distancias = scipy.spatial.distance.cdist(descriptores_imagen1, descriptores_imagen2, metric='euclidean')
		# Ordenamos el array de distancias para poder aplicar el criterio nddr
		distancias_ordenadas = np.sort(distancias)
		# Creamos una lista vacia en la que guardaremos los indices asignados
		indices_asignados = []
		# Realizamos un bucle en el cual asignaremos las correspondencias siempre y cuando se cumpla el criterio nddr
		for i, lista in enumerate(distancias_ordenadas):
			# Primero comprobamos si el punto ya ha sido asignado
			# Para ello obtenemos el indice de la distancia minima
			indice = np.where(distancias[i] == lista[0])[0][0]
			# Comprobamos si este no ha sido asignado
			if indice not in indices_asignados:
				# Primero evaluamos el primer descriptor con el resto
				# Comprobamos si la distancia minima es menor a la que se recive por argumentos
				if lista[0] < max_distancia:
					# Si la distancia cumple con el primer requisito pasamos a comprobar si cumple con el criterio nddr
					# Comprobamos si la segunda distancia mas corta es 0, si lo es simplemente aniadimos a correspondencias
					if lista[1] == 0:
						# Si es la primera correspondencia que se aniade
						if flag_vacio == 0:	
							correspondencias = []
							flag_vacio = 1
						# Aniadimos el indice a la lista de indices asignados
						indices_asignados.append(indice)
						# Guardamos la correspondencia en forma de tupla con los dos indices
						correspondencias.append((i, indice))
					# Si el indice no vale 0 pasamos a realizar las comprobaciones pertinentes
					else:
						# Obtenemos el coeficiente nddr
						nddr = lista[0]/lista[1]
						# Comprobamos si el nddr obtenido es menor que el recibido por argumento
						if nddr < max_nndr:
							# Una vez realizadas las comprobaciones, pasamos a evaluar si el resto de puntos tienen
							# una distancia menor
							# Ponemos una flag a 0 para realizar esta comprobacion
							flag = 0
							for j, lista_test in enumerate(distancias_ordenadas[i + 1: ]):
								# Guardamos en un indice auxiliar el indice con el que se corresponde la distancia con la que
								# realizamos las comprobaciones
								indice_aux = np.where(distancias[i + j + 1] == lista_test[0])[0][0]
								# Si la distancia es no es menor que el resto de distancias y ambas se corresponden al mismo indice
								# ponemos la flag a 1 para no asignar la correspondencia
								if lista_test[0] < lista[0] and indice_aux == indice:
									# Ponemos la flag a 1
									flag = 1
							# Si la flag esta a 0 anadimos la correspondencia
							if flag == 0:
								# Si es la primera correspondencia que se aniade
								if flag_vacio == 0:	
									correspondencias = []
									flag_vacio = 1
								# Aniadimos el indice a la lista de indices asignados
								indices_asignados.append(indice)
								# Guardamos la correspondencia en forma de tupla con los dos indices
								correspondencias.append((i, indice))	

	# Convertimos el array de correspondencias a numpy array y lo devolvemos
	return np.array(correspondencias, dtype = np.int64)

# Función de test creada por Pablo Hergueta y Tomás Higuera
#
# Imprime los puntos de interés obtenidos con el detector de Harris en las
# dos versiones de la imagen cuyo nombre se pasa por argumento.
#
# ags:
#	filename - nombre del archivo 
def test(filename, tipoDist, tipoDesc):
	# Generamos el nombre de las dos rutas
	filename1 = 'img/filename1.jpg'
	filename2 = 'img/filename2.jpg'

	# Obtenemos las dos imágenes del sistema de ficheros
	imgA = io.imread(filename1.replace("filename", filename))
	imgB = io.imread(filename2.replace("filename", filename))

	# Pasamos las imágenes a escala de grises para poder encontrar los puntos de interés
	imgAgray = color.rgb2gray(imgA)
	imgBgray = color.rgb2gray(imgB)

	# Obtenemoms los puntos de interés con el detector de Harris desarrollado en esta práctica
	puntos_interesA = detectar_puntos_interes_harris(imgAgray)
	puntos_interesB = detectar_puntos_interes_harris(imgBgray)

	# Genereamos los descriptores de cada una de las imágenes con los puntos de interés que ya conocemos
	descriptoresA, new_coordsA = descripcion_puntos_interes(imgAgray, puntos_interesA, tipoDesc = tipoDesc)
	descriptoresB, new_coordsB = descripcion_puntos_interes(imgBgray, puntos_interesB, tipoDesc = tipoDesc)

	# Finalmente, hayamos las correspondencias entre los puntos de interés de ambas imágenes
	correspondencias = correspondencias_puntos_interes(descriptoresA, descriptoresB, tipoDist = tipoDist)

	# Generamos la figura que contendrá las dos imágenes con sus puntos de interés
	fig, ax = plt.subplots(nrows=1, ncols=1)
	plt.gray()
	plot_matches(ax, imgA, imgB, puntos_interesA, puntos_interesB, correspondencias)
	ax.axis("off")
	ax.set_title("Correspondecias de puntos de interés {}".format(filename))
	# Mostramos la imagen
	plt.show(block=True)

	
if __name__ == "__main__":
	cls()
	print("TSV - Practica 2 - Parte 3\n" +
			"Realizando tests para las funciones de la parte 3\n" +
			"Las funciones seran correctas si los resultados obtenidos\n" +
			"tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")    
	np.set_printoptions(precision=PRECISION)

	# ejecucion de la funcion test y mostrar resultado de los tests por pantalla    
	disptime = -1
	print("Tests completados = " + str(test_p2_tarea3(disptime=disptime,tipoDesc='hist',tipoDist='nndr'))) 

	# Ejecución de la función de test desarrollada por los alumnos
	# Nombre de los ficheros válidos: "NotreDame", "EGaudi_"
	test("NotreDame", tipoDesc='hist', tipoDist = 'nndr')        