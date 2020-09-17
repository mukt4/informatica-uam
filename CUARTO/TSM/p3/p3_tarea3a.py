# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 2: Extracción de características. Puntos de Interés.
# Parte 3: Reconocimiento de Escenas utilizando "Bag of Words" features y clasificador
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
from p3_utils import PRECISION, cls, test_p3_tarea3a
import cv2 as cv

# Path del dataset
# data_path = '../Practica 3/data'
data_path = './data'


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
	svm.setKernel(cv.ml.SVM_RBF)
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
	print("TSV - Practica 3 - Parte 3\n" +
	  "Realizando tests para las funciones de la parte 3\n" +
	  "Las funciones seran correctas si los resultados obtenidos\n" +
	  "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")
	np.set_printoptions(precision=PRECISION)

	# ejecucion de la funcion test y mostrar resultado de los tests por pantalla
	print("Tests completados = " + str(test_p3_tarea3a()))
