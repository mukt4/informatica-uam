# Tratamiento de Seniales Visuales @ EPS-UAM
# Practica 1: Fusion de imagenes mediante piramides
# Tarea 5a:  fusion de imagenes RGB mediante piramides
import matplotlib.pyplot as plt
import numpy as np
import math
import sys
from cv2 import cv2

from skimage import io, color, img_as_float32

from p1_utils import visualizar_gaus_piramide, visualizar_lapl_piramide
import p1_tarea1
import p1_tarea2
import p1_tarea3
from p1_tarea2 import gaus_piramide, lapl_piramide
from p1_tarea3 import fusionar_lapl_pyr, reconstruir_lapl_pyr

def run_fusion_rgb(imgA, imgB, mask, niveles):
	# Esta funcion implementa la fusion de dos imagenes calculando las 
	# piramides Laplacianas de las imagenes de entrada y la piramide
	# Gausiana de una mascara.
	#
	# Debe realizar las siguientes operaciones:
	# 1. Verificar que las imagenes son matrices bidimensionales (i.e. escala de grises)
	#    pues esta funcion no procesa imagenes RGB. La funcion debe indicar un error en caso positivo
	# 2. Convertir las imagenes y mascara a tipo float 
	# 3. Normalizar todas las imagenes/mascara tipo float en el rango [0,1]
	# 4. Calcular las piramides Gaussianas de los datos convertidos a float 
	#    utilizando la funcion "gaus_piramide"
	# 5. Calcular las piramides Laplacianas de las imagenes
	#    utilizando la funcion "lapl_piramide"
	# 6. Fusionar las piramides Laplacianas de las imagenes 
	#    y la Gaussiana de la mascara con la funcion "fusionar_lapl_pyr"
	# 7. Reconstruir la piramide resultante para obtener una imagen 
	#    con la funcion "reconstruir_lapl_pyr"
	# 8. Tras la reconstruccion, algunos valores pueden estar fuera de rango (<0 o >255).
	#    En caso positivo, recortar a '0' o '1' segun corresponda. 
	#    Ademas el tipo de imagen de salida debe ser float
	#
	# Argumentos de entrada:
	#   imgA: numpy array de tamanio [imagen_height, imagen_width].
	#   imgB: numpy array de tamanio [imagen_height, imagen_width].
	#   mask: numpy array de tamanio [imagen_height, imagen_width].
	#
	# Devuelve:
	#   Gpyr_imgA: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Gaussiana de la imagen A
	#   Gpyr_imgB: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Gaussiana de la imagen B
	#   Gpyr_mask: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Gaussiana de la mascara
	#   Lpyr_imgA: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Laplaciana de la imagen A
	#   Lpyr_imgB: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Laplaciana de la imagen B
	#   Lpyr_fus: lista de numpy arrays con variable tamanio con "niveles+1" elementos 
	#               correspodientes a la piramide Laplaciana de la fusion imagen A & B
	#   Lpyr_fus_rec:  numpy array de tamanio [imagen_height, imagen_width] correspondiente
	#               a la reconstruccion de la piramide Lpyr_fus
		
	# iniciamos las variables de salida    
	Gpyr_imgA = []      # Piramide Gaussiana imagen A
	Gpyr_imgB = []      # Piramide Gaussiana imagen B
	Gpyr_mask = []      # Piramide Gaussiana mascara    
	Lpyr_imgA = []      # Piramide Laplaciana imagen A
	Lpyr_imgB = []      # Piramide Laplaciana imagen B
	Lpyr_fus = []       # Piramide Laplaciana fusionada
	Lpyr_fus_rec = []   # Imagen reconstruida de la piramide Laplaciana fusionada
	Lpyr_fus_rec_list = [] # Lista de canales de la imagen reconstruida
	
	if len(imgA.shape) != 3 or len(imgB.shape) != 3:
		raise ValueError("ERROR - Las imagenes no son RGB.")

	for i in range(3):
		floatCanalA = img_as_float32(imgA[:, :, i])
		floatCanalB = img_as_float32(imgB[:, :, i])
		floatMask = img_as_float32(mask[:, :, i])

		Gpyr_imgA = gaus_piramide(floatCanalA, niveles)
		Gpyr_imgB = gaus_piramide(floatCanalB, niveles)
		Gpyr_mask = gaus_piramide(floatMask, niveles)

		Lpyr_imgA = lapl_piramide(Gpyr_imgA)
		Lpyr_imgB = lapl_piramide(Gpyr_imgB)

		Lpyr_fus = fusionar_lapl_pyr(Lpyr_imgA, Lpyr_imgB, Gpyr_mask)

		Lpyr_fus_rec = reconstruir_lapl_pyr(Lpyr_fus)

		Lpyr_fus_rec_list.append(np.clip(Lpyr_fus_rec, 0, 1))
	
	Lpyr_fus_rec = cv2.merge(Lpyr_fus_rec_list, 3)

	return Lpyr_fus_rec

if __name__ == "__main__":
	#tests las tareas
	print('Testeando fusion en rgb')
 
	#ruta de las imagenes
	filename1 = 'img/orchid.jpg'
	filename2 = 'img/violet.jpg'
	filemask = 'img/orchid_mask.jpg'

	# lectura de imagenes     
	imgA = io.imread(filename1)
	imgB = io.imread(filename2)
	mask = io.imread(filemask)

	# comprobacion de errores
	assert imgA.shape == imgB.shape and imgA.shape == mask.shape, "Error - los tamanios de imagenes y mascara son distintos"
	
	imgA32 = img_as_float32(imgA)
	imgB32 = img_as_float32(imgB)
	mask32 = img_as_float32(mask)

	fus_img = imgA32 * mask32 + (1 - mask32) * imgB32

	# ejecutamos la fusion con piramides
	niveles = 4
	Lpyr_fus_rec = run_fusion_rgb(imgA, imgB, mask, niveles)

	# mostrar imagenes resultado
	fig, ax = plt.subplots( nrows = 2, ncols = 3, figsize = (8, 5), sharex = True , sharey = True )
	plt.gray()

	ax[0,0].imshow (imgA32)
	ax[0,0].axis('off')
	ax[0,0].set_title ('Imagen 1')

	ax[0,1].imshow ( imgB32 )
	ax[0,1].axis ('off')
	ax[0,1].set_title ('Imagen 2')

	ax[0,2].imshow (mask32)
	ax[0,2].axis('off')
	ax[0,2].set_title ('Máscara')

	ax[1,0].imshow ( fus_img )
	ax[1,0].axis ('off')
	ax[1,0].set_title ('Fusion sin piramides')

	ax[1,1].imshow ( Lpyr_fus_rec )
	ax[1,1].axis ('off')
	ax[1,1].set_title ('Fusion con piramides (' + str(niveles) + ' niveles)')

	ax[1,2].axis ('off')

	fig.tight_layout()
	plt.show (block = False)

	print("Test de fusion con diferentes fotos")

	#ruta de las imagenes
	filename1 = 'img/apple2.jpg'
	filename2 = 'img/orange2.jpg'
	filemask = 'img/mask_apple2_orange2.jpg'

	# lectura de imagenes     
	imgA = io.imread(filename1)
	imgB = io.imread(filename2)
	mask = io.imread(filemask)

	# comprobacion de errores
	assert imgA.shape == imgB.shape and imgA.shape == mask.shape, "Error - los tamanios de imagenes y mascara son distintos"
	
	imgA32 = img_as_float32(imgA)
	imgB32 = img_as_float32(imgB)
	mask32 = img_as_float32(mask)

	fus_img = imgA32 * mask32 + (1 - mask32) * imgB32

	# ejecutamos la fusion con piramides
	niveles = 4
	Lpyr_fus_rec = run_fusion_rgb(imgA, imgB, mask, niveles)

	# mostrar imagenes resultado
	fig, ax = plt.subplots( nrows = 2, ncols = 3, figsize = (8, 5), sharex = True , sharey = True )
	plt.gray()

	ax[0,0].imshow (imgA32)
	ax[0,0].axis('off')
	ax[0,0].set_title ('Imagen 1')

	ax[0,1].imshow ( imgB32 )
	ax[0,1].axis ('off')
	ax[0,1].set_title ('Imagen 2')

	ax[0,2].imshow (mask32)
	ax[0,2].axis('off')
	ax[0,2].set_title ('Máscara')

	ax[1,0].imshow ( fus_img )
	ax[1,0].axis ('off')
	ax[1,0].set_title ('Fusion sin piramides')

	ax[1,1].imshow ( Lpyr_fus_rec )
	ax[1,1].axis ('off')
	ax[1,1].set_title ('Fusion con piramides (' + str(niveles) + ' niveles)')

	ax[1,2].axis ('off')

	fig.tight_layout()
	plt.show (block = False)

	# Tests realizados por Tomas Higuera y Pablo Hergueta
	print("Tests de fusion con piramides de diferentes niveles:")
	Lpyr_fus_rec3 = run_fusion_rgb(imgA, imgB, mask, 3)

	Lpyr_fus_rec40 = run_fusion_rgb(imgA, imgB, mask, 40)

	Lpyr_fus_rec2 = run_fusion_rgb(imgA, imgB, mask, 2)

    # mostrar imágenes resultado
	fig, ax = plt.subplots( nrows = 2, ncols = 2, sharex = True , sharey = True )
	plt.gray()
    
	ax[0,0].imshow (Lpyr_fus_rec2)
	ax[0,0].axis('off')
	ax[0,0].set_title ('Fusion de 2 niveles')

	ax[0,1].imshow (Lpyr_fus_rec3)
	ax[0,1].axis ('off')
	ax[0,1].set_title ('Fusion de 3 niveles')

	ax[1,0].imshow (Lpyr_fus_rec)
	ax[1,0].axis('off')
	ax[1,0].set_title ('Fusion de 4 niveles')

	ax[1,1].imshow (Lpyr_fus_rec40)
	ax[1,1].axis ('off')
	ax[1,1].set_title ('Fusion de 40 niveles')

	fig.tight_layout()
	plt.show(block = False)

	#ruta de las imagenes
	filename1 = 'img/apple1.jpg'
	filename2 = 'img/orange1.jpg'
	filemask = 'img/mask_apple1_orange1.jpg'

	# lectura de imagenes     
	imgA = io.imread(filename1)
	imgB = io.imread(filename2)
	mask = io.imread(filemask)

	# comprobacion de errores
	assert imgA.shape == imgB.shape and imgA.shape == mask.shape, "Error - los tamanios de imagenes y mascara son distintos"
	
	imgA32 = img_as_float32(imgA)
	imgB32 = img_as_float32(imgB)
	mask32 = img_as_float32(mask)

	fus_img = imgA32 * mask32 + (1 - mask32) * imgB32

	# ejecutamos la fusion con piramides
	niveles = 4
	Lpyr_fus_rec = run_fusion_rgb(imgA, imgB, mask, niveles)

	# mostrar imagenes resultado
	fig, ax = plt.subplots( nrows = 2, ncols = 3, figsize = (8, 5), sharex = True , sharey = True )
	plt.gray()

	ax[0,0].imshow (imgA32)
	ax[0,0].axis('off')
	ax[0,0].set_title ('Imagen 1')

	ax[0,1].imshow ( imgB32 )
	ax[0,1].axis ('off')
	ax[0,1].set_title ('Imagen 2')

	ax[0,2].imshow (mask32)
	ax[0,2].axis('off')
	ax[0,2].set_title ('Máscara')

	ax[1,0].imshow ( fus_img )
	ax[1,0].axis ('off')
	ax[1,0].set_title ('Fusion sin piramides')

	ax[1,1].imshow ( Lpyr_fus_rec )
	ax[1,1].axis ('off')
	ax[1,1].set_title ('Fusion con piramides (' + str(niveles) + ' niveles)')

	ax[1,2].axis ('off')

	fig.tight_layout()
	plt.show ()

	# Test de fusion con nuestras fotos 
	print("Probando fusion con nuestras fotos")

	#ruta de las imagenes
	filename1 = 'img/cat.jpg'
	filename2 = 'img/dog.jpg'
	filemask = 'img/resize_mask2.jpg'

	# lectura de imagenes     
	try:
		imgA = io.imread(filename1)
		imgB = io.imread(filename2)
		mask = io.imread(filemask)

		# comprobacion de errores
		assert imgA.shape == imgB.shape and imgA.shape == mask.shape, "Error - los tamanios de imagenes y mascara son distintos"
		
		imgA32 = img_as_float32(imgA)
		imgB32 = img_as_float32(imgB)
		mask32 = img_as_float32(mask)

		fus_img = imgA32 * mask32 + (1 - mask32) * imgB32

		# ejecutamos la fusion con piramides
		niveles = 4
		Lpyr_fus_rec = run_fusion_rgb(imgA, imgB, mask, niveles)

		# mostrar imagenes resultado
		fig, ax = plt.subplots( nrows = 2, ncols = 3, figsize = (8, 5), sharex = True , sharey = True )
		plt.gray()

		ax[0,0].imshow (imgA32)
		ax[0,0].axis('off')
		ax[0,0].set_title ('Imagen 1')

		ax[0,1].imshow ( imgB32 )
		ax[0,1].axis ('off')
		ax[0,1].set_title ('Imagen 2')

		ax[0,2].imshow (mask32)
		ax[0,2].axis('off')
		ax[0,2].set_title ('Máscara')

		ax[1,0].imshow ( fus_img )
		ax[1,0].axis ('off')
		ax[1,0].set_title ('Fusion sin piramides')

		ax[1,1].imshow ( Lpyr_fus_rec )
		ax[1,1].axis ('off')
		ax[1,1].set_title ('Fusion con piramides (' + str(niveles) + ' niveles)')

		ax[1,2].axis ('off')

		fig.tight_layout()
		plt.show ()
	except OSError:
		print("No se puede realizar la fusion ya que no se dispone de las fotos: " + filename1, filename2, filemask)
