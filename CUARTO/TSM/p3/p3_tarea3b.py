# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 2: Extracción de características. Puntos de Interés.
# Parte 3b: Reconocimiento de Escenas utilizando "Bag of Words" features y clasificador
# SVM

# librerias y paquetes por defecto
import numpy as np
import scipy
from skimage.io import imread
from skimage.color import rgb2gray
from skimage.feature import hog
from skimage.transform import resize
from scipy.spatial.distance import cdist
from collections import Counter
from p3_utils import PRECISION, cls, test_p3_tarea3b
from skimage import img_as_float64
import cv2 as cv
from scipy.cluster.vq import kmeans

# Path del dataset
# data_path = '../Practica 3/data'
data_path = './data'


def obtener_features_hog(path_imagenes, celdas_bloque=4, tamano=100):
	# Computar Histograma de Gradientes Orientados (HOG) para una lista de imagenes.
	# Para cada imagen obtenemos N descriptores de tamaño D.
	#
	# Argumentos de entrada:
	#   - path_imagenes: Una lista de Python 1-D de strings. Cada string se corresponde
	#                  con el path de la imagen en el sistema.
	#
	# Argumentos de salida:
	#   - todos_hog: Lista de numpy arrays (NxD) que contiene las features HOG para cada
	#                imagen. N es el numero de features y D el numero de dimensiones de la feature.
	#                Para una imagen de 100x100 deberiamos de obtener 484 features de 144 dimensiones.
	# Iniciamos variable de salida
	todos_hog = []

	# Para cada imagen
	for filename in path_imagenes:
		# Obtenemos la imagen del sistema de archivos
		img = imread(filename)

		# Convertimos la imagen a escala de grises
		img = rgb2gray(img)

		# Convertimos imagen a tipo float y normalizamos en [0,1]
		img = img_as_float64(img)

		# Redimensionamos imagen de acuerdo con el tamaño pasado por argumento
		img = resize(img, (tamano, tamano), anti_aliasing=True)

		# Generamos el histograma de gradientes orientados de la imagen
		descriptor = hog(img, orientations=9, pixels_per_cell=(4, 4), cells_per_block=(celdas_bloque, celdas_bloque), feature_vector=True)	

		# Hacemos reshape para obtener el histograma con las dimensiones correctas
		descriptor_correcto = np.reshape(descriptor, (-1, 9 * celdas_bloque ** 2))

		# Añadimos el histograma al conjunto de descriptores
		todos_hog.append(descriptor_correcto)

	return todos_hog


def construir_vocabulario(path_imagenes, tamano_vocab, iter=1):
	# Esta funcion calcula descriptores HOG para una serie de imagenes
	# y utiliza K-Means para clusterizar los descriptores en "tamano_vocab" clusters.
	#
	# Argumentos de entrada:
	#   - path_imagenes: Una lista de Python 1-D de strings. Cada string se corresponde
	#                    con el path de la imagen en el sistema.
	#   - tamano_vocab: Numero de palabras para el vocabulario a construir.
	#
	# Argumentos de salida:
	#   - vocabulario: Numpy array que contiene los centros de los clusters obtenidos por K-Means
	#                      Tamaño -> (tamano_vocab, tamaño_descriptor_hog)

	# Iniciamos variable de salida
	vocabulario = []

	# Generamos todos los histogramas de las imágenes
	features_hog = obtener_features_hog(path_imagenes)
	
	# Convertimos la lista de descriptores al formato correcto (bidimensional) de array para poder utilizar kmeans
	features_hog = np.concatenate(features_hog, axis=0)

	# Ejecutamos kmeans para el set de histogramas formando un número (tamano_vocab) de clusters (o palabras)
	vocabulario, distortion = kmeans(features_hog, tamano_vocab, iter=iter)

	# Guardamos el vocabulario en un fichero auxiliar para ahorrar coste computacional
	np.save(data_path + "/vocab.npy", vocabulario)

	return vocabulario


def obtener_bags_of_words(path_imagenes):
	# Obtener Histograma Bag of Words para cada imagen
    #
    # Argumentos de entrada:
    #   - path_imagenes: Una lista de Python 1-D de strings. Cada string se corresponde
    #                    con el path de la imagen en el sistema.
    #
    # Argumentos de salida:
    #   - bag_of_words: Array de Numpy [N x D]. Un array de tamaño [N x D], donde N es el
    #                   numero de imagenes en path_imagenes y D es el tamaño del histograma
    #                   construido para cada imagen.
    #
    # IMPORTANTE: Esta funcion hace uso del vocabulario precalculado en la funcion "construir_vocabulario"
    # Para que la ejecucion del código sea mas rápida este vocabulario se guarda en un fichero .npy en la
    # carpeta "Data". Es necesario cargarlo usando la siguiente instruccion:
    #       - vocab = np.load(data_path + '/vocab.npy', allow_pickle=True)
    # Iniciamos la variable de salida
    bag_of_words = []

    # Obtenemos el vocabulario ya construido
    vocab = np.load(data_path + '/vocab.npy', allow_pickle=True)

    # Calculamos los descriptores de todas las imagenes
    features_hog = obtener_features_hog(path_imagenes)

    # Recorremos los descriptores de cada imagen
    for feature in features_hog:
        # Calculamos las distancias entre cada descriptor (histograma) y el vocabulario
        # Suma total de las distancias
        total = 0
        # Calculamos las distancias de las imagenes con las distintas palabras del vocabulario
        distancias = cdist(feature, vocab)
        # Declaramos el histograma en cada uno de los features
        histograma = np.zeros(vocab.shape[0])
        # Recorremos las distancias
        for i, distancia in enumerate(distancias):
            # Ordenamos el array de distancias que vamos a comprobar
            # Realizamos el ordenamiento utilizando argsort, para tener directamente los valores 
            # de los indices con distancia mas corta
            distancia_ordenada = np.argsort(distancia)
            # Obtenemos el indice al que se corresponde esa distancias
            indice = distancia_ordenada[0]
            # Aumentamos la distancia para luego normalizar y obtener mejores probabilidades
            histograma[indice] += distancia[indice]
            # Aumentamos el total de distancia para despues normalizar
            total += distancia[indice]
        # Dividimos el histograma entre el total para normalizar
        bag_of_words.append(histograma/total)

    # Devolvemos el bag of words
    return np.vstack(bag_of_words)

def clasificador_svm(caracteristicas_train, caracteristicas_test, etiquetas_train, categories):
	# Entranar un clasificador SVM y clasificar unas caracteristicas de test
	#
	# Argumentos de entrada:
	#   - caracteristicas_train: Numpy array de tamaño [N x D]. Siendo N el numero de imagenes
	#                            de entrenamiento y D el numero de caracteristicas.
	#   - caracteristicas_test: Numpy array de tamaño [N x D]. Siendo N el numero de imagenes
	#                            de evaluacion y D el numero de caracteristicas.
	#   - etiquetas_train: Lista de python con N elementos que contiene las etiquetas de las
	#                      imagenes de entrenamiento.
	#
	# Argumentos de salida:
	#   - predicciones:  array (M x 1) de categorias. Etiquetas predichas por la SVM para cada
	#                    imagen de test.

	# Iniciamos variable de salida
	predicciones = []

	# Documentación: https://docs.opencv.org/3.4/d1/d73/tutorial_introduction_to_svm.html
	# Elegimos los parámetros para la SVM de acuerdo con lo especificado en el enunciado de la asignatura		
	svm = cv.ml.SVM_create()
	svm.setType(cv.ml.SVM_C_SVC)
	svm.setKernel(cv.ml.SVM_CHI2)
	svm.setTermCriteria((cv.TERM_CRITERIA_MAX_ITER, 200, 1e-6))

	# Preparamos los parámetros para la función train de acuerdo con la documentación de OpenCV
	etiquetas_train = np.array(etiquetas_train)
	caracteristicas_train = np.matrix(caracteristicas_train, dtype=np.float32)

	# Documentación: https://docs.opencv.org/3.4/db/d7d/classcv_1_1ml_1_1StatModel.html#af96a0e04f1677a835cc25263c7db3c0c
	svm.train(caracteristicas_train, cv.ml.ROW_SAMPLE, etiquetas_train)
	
	# Preparamos las imágenes de evaluación para que puedan usarse en la fución de predicción con SVMs de OpenCV
	caracteristicas_test = np.matrix(caracteristicas_test, dtype=np.float32)

	# Clasificamos las imágenes de caracteristicas_test utilizando la máquina de vectores de soporte ya entrenada
	results = svm.predict(caracteristicas_test)[1]
	
	for result in results:
		# Conseguimos cada una de las predicciones obtenidas
		prediccion = result[0]

		# Pasamos el valor de a predicción a tipo int para poder indexar las categorías con él
		prediccion = prediccion.astype(np.int)

		# Añadimos al conjunto de predicciones la categoría correspondiente,
		# que conocemos indexando la predicción obtenida al hacer la clasificación)
		predicciones.append(categories[prediccion])

	return predicciones


if __name__ == "__main__":
	cls()
	print("TSV - Practica 3b - Parte 3\n" +
		  "Obteniendo precision para el sistema implementado en la parte 3b\n" +
		  "EN ESTE SCRIPT NO SE EJECUTA NINGUN TEST SOBRE SUS RESULTADOS.\n")
	np.set_printoptions(precision=PRECISION)

	# ejecucion de la que obtiene la precision del modelo
	test_p3_tarea3b()
