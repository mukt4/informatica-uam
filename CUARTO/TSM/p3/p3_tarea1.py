# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 3: Reconocimiento de Escenas
# Parte 1: Reconocimiento de Escenas utilizando "Tiny Image" features y clasificador
# K-Nearest-Neighbors(KNN)

# librerias y paquetes por defecto
import numpy as np
import scipy
from skimage.io import imread
from skimage.color import rgb2gray
from skimage.transform import resize
from scipy.spatial.distance import cdist
from p3_utils import PRECISION, cls, test_p3_tarea1
from skimage import img_as_float64
from collections import Counter
from operator import itemgetter

# Path del dataset
# data_path = '../Practica 3/data'
data_path = './data'

def obtener_tiny_images(path_imagenes, tamano=16):
	# Esta funcion redimensiona una serie de imagenes, vectoriza cada imagen redimensionada
	# y posteriormente forma una matriz con todos estos vectores
	#
	# Argumentos de entrada:
	#   - path_imagenes: Una lista de Python 1-D de strings. Cada string se corresponde
	#                    con el path de la imagen en el sistema.
	#
	# Argumentos de salida:
	#   - array_imagenes: numpy array de tamaño [N, D]. N es el numero de imagenes y
	#                     D es la longitud del vector de representacion. Ejemplo:
	#                     si las imagenes son 16x16 D será 16 * 16 = 256

	# Iniciamos variable de salida
	array_imagenes = []

	for filename in path_imagenes:
		# Obtenemos la imagen del sistema de archivos
		img = imread(filename)

		# Convertimos la imagen a escala de grises
		img = rgb2gray(img)

		# Convertimos imagen a tipo float y normalizamos en [0,1]
		img = img_as_float64(img)

		# Redimensionamos imagen de acuerdo con el tamaño pasado por argumento
		img = resize(img, (tamano, tamano), anti_aliasing=True)

		# Añadimos la imagen resultante vectorizada a la salida
		array_imagenes.append(img.flatten())
		
	return np.asarray(array_imagenes)


def clasificador_nearest_neighbor(caracteristicas_train, caracteristicas_test, etiquetas_train, k=3):
	# Prediccion de las clases de cada imagen utilizando un clasificador K-Nearest-Neighbors(KNN)
	#
	# Argumentos de entrada:
	#   - caracteristicas_train: Numpy array (N x D). Feature matrix for training.
	#   - caracteristicas_test: Feature matrix for testing.
	#   - etiquetas_train: list(). Python list of length N.
	#   - k: int. Numero de Vecinos mas cercanos para clasificar
	#
	# Argumentos de salida:
	#   - predicciones:  array (M x 1) of strings. Predicted labels for the test images
	
	# Iniciamos variable de salida
	predicciones = []

	# Calculamos las distancias entre las imágenes que tenemos etiquetadas y las que queremos testear
	distancias = cdist(caracteristicas_test, caracteristicas_train, metric="euclidean")

	# Para cada imagen
	for distancia in distancias:
		# Ordenamos todas las distancias de menor a mayor
		distancia_ordenada = np.sort(distancia)

		# Guardamos las k distancias menores (a los vecinos)
		knn = distancia_ordenada[:k]

		# Variable para guardar los índices de los vecinos cercanos
		indices_cercanos = []

		# Obtenemos los índices en el array original de los vecinos próximos
		for aux in knn:
			aux = np.where(distancia == aux)[0][0]
			indices_cercanos.append(aux)

		# En caso de que k = 1, devolvemos el primer y único vecino
		if k == 1:
			etiquetas_prediccion = etiquetas_train[indices_cercanos[0]]
			predicciones.append(etiquetas_prediccion)

		# En el resto de casos
		else:
			# Averiguamos las etiquetas que corresponden a los vecinos próximos
			etiquetas_cercanas = itemgetter(*indices_cercanos)(etiquetas_train)

			# Creamos un diccionario con la frecuencia de cada etiqueta (con k entradas)
			cuenta_etiquetas = Counter(etiquetas_cercanas)

			# Obtenemos la etiqueta cuyo valor (frecuencia) sea mayor
			etiqueta_prediccion = max(cuenta_etiquetas, key=cuenta_etiquetas.get)

			# Añadimos la etiqueta predecida para cada imagen analizada
			predicciones.append(etiqueta_prediccion)

	return np.asarray(predicciones)


if __name__ == "__main__":
	# cls()
	print("TSV - Practica 3 - Parte 1\n" +
	  "Realizando tests para las funciones de la parte 1\n" +
	  "Las funciones seran correctas si los resultados obtenidos\n" +
	  "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")
	np.set_printoptions(precision=2)

	# ejecucion de la funcion test y mostrar resultado de los tests por pantalla
	print("Tests completados = " + str(test_p3_tarea1()))
