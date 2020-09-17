# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 2: Extracción de características. Puntos de Interés.
# Parte 1: Detección de puntos de interés con Harris corner detector.

# librerias y paquetes por defecto
import numpy as np
import scipy
import scipy.ndimage as nd
from skimage.feature import corner_peaks, peak_local_max
from p2_utils import PRECISION, cls, test_p2_tarea1
from skimage import img_as_float32, color, io
import matplotlib.pyplot as plt


def detectar_puntos_interes_harris(imagen, sigma=1, k=0.05):
	# Esta funcion detecta puntos de interes en una imagen dada.
	#
	# Para realizar la deteccion de puntos de interes se utilizara el detector
	# de esquinas de Harris.
	#
	# Argumentos de entrada:
	#   - imagen: numpy array de tamaño [imagen_height, imagen_width].
	#   - sigma: valor de tipo double o float que determina el factor de suavizado aplicado
	#   - k: valor de tipo double o float que determina la deteccion de Harris
	# Argumentos de salida
	#   - coords_esquinas: numpy array de tamaño [num_puntos_interes, 2] con las coordenadas
	#                      de los puntos de interes detectados en la imagen.
	#
	# NOTA: no modificar los valores por defecto de las variables de entrada sigma y k,
	#       pues se utilizan para verificar el correcto funciomaniento de esta funcion
	coords_esquinas = []  # iniciamos variable de salida
	# Convertimos la imagen al formato float acotado en [0,1]
	imagen_float = img_as_float32(imagen)
	# Aplicamos los filtros de sobel para calcular las derivadas parciales
	# Calculamos la derivada parcial de X
	imagen_dx = nd.sobel(imagen_float, axis=0, mode='constant')
	# Calculamos la derivada parcial de Y
	imagen_dy = nd.sobel(imagen_float, axis=1, mode='constant')
	# Calulamos la derivada de X al cuadrado
	imagen_dx2 = np.multiply(imagen_dx, imagen_dx)
	# Calculamos la derivada de Y al cuadrado
	imagen_dy2 = np.multiply(imagen_dy, imagen_dy)
	# Calculamos la derivada de Y por la derivada de X
	imagen_dxy = np.multiply(imagen_dx, imagen_dy)
	# Aplicamos filtrado gaussiano a las tres imagenes producto obtenidas
	imagen_dx2_gauss = nd.gaussian_filter(
		imagen_dx2, sigma=sigma, mode='constant')

	imagen_dy2_gauss = nd.gaussian_filter(
		imagen_dy2, sigma=sigma, mode='constant')

	imagen_dxy_gauss = nd.gaussian_filter(
		imagen_dxy, sigma=sigma, mode='constant')

	# Calculamos el determinante de la MATRIZxy
	determinante = (imagen_dx2_gauss * imagen_dy2_gauss) - \
		(imagen_dxy_gauss ** 2)
	# Calculamos el valor de la traza
	traza = imagen_dx2_gauss + imagen_dy2_gauss
	# Calculamos la funcion de respuesta R
	R = determinante - k * (traza ** 2)

	# Detectamos esquinas umbralizando función R
	coords_esquinas = corner_peaks(R, min_distance=5, threshold_rel=0.2)

	return coords_esquinas


# Función de test creada por Pablo Hergueta y Tomás Higuera
#
# Imprime los puntos de interés obtenidos con el detector de Harris en las
# dos versiones de la imagen cuyo nombre se pasa por argumento.
#
# ags:
#	filename - nombre del archivo 
def test(filename):
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

	# Generamos la figura que contendrá las dos imágenes con sus puntos de interés
	fig = plt.figure(figsize=(8,8))
	plt.clf()
	# Imagen 1
	fig.add_subplot(1, 2, 1)
	plt.imshow(imgA)
	# Colocamos los puntos de interés del array obtenido anteriormente sobre la imagen original (a color)
	plt.plot(puntos_interesA[:, 1], puntos_interesA[:, 0], '+', linewidth=4, color='red')
	# Imagen 2
	fig.add_subplot(1, 2, 2)
	plt.imshow(imgB)
	# Colocamos los puntos de interés del array obtenido anteriormente sobre la imagen original (a color)
	plt.plot(puntos_interesB[:, 1], puntos_interesB[:, 0], '+', linewidth=4, color='red')
	# Mostramos la imagen
	plt.show(block=True)


if __name__ == "__main__":
	cls()
	print("TSV - Practica 2 - Parte 1\n" +
		  "Realizando tests para las funciones de la parte 1\n" +
		  "Las funciones seran correctas si los resultados obtenidos\n" +
		  "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")
	np.set_printoptions(precision=PRECISION)

	# ejecucion de la funcion test y mostrar resultado de los tests por pantalla
	print("Tests completados = " + str(test_p2_tarea1(disptime=-1)))

	# Ejecución de la función de test desarrollada por los alumnos
	# Nombre de los ficheros válidos: "NotreDame", "EGaudi_", "Mount_Rushmore"
	test("NotreDame")
