# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 1: Fusion de imagenes mediante piramides
# Tarea 4: fusion de imagenes en niveles de gris mediante piramides
import matplotlib.pyplot as plt
import numpy as np
import math
import sys

from skimage import io, color, img_as_float32

from p1_utils import visualizar_gaus_piramide, visualizar_lapl_piramide
import p1_tarea1
import p1_tarea2
import p1_tarea3
from p1_tarea2 import gaus_piramide, lapl_piramide
from p1_tarea3 import fusionar_lapl_pyr, reconstruir_lapl_pyr

def run_fusion(imgA, imgB, mask, niveles):
    # Esta funcion implementa la fusion de dos imagenes calculando las 
    # pirámides Laplacianas de las imagenes de entrada y la pirámide
    # Gausiana de una mascara.
    #
    # Debe realizar las siguientes operaciones:
    # 1. Verificar que las imagenes son matrices bidimensionales (i.e. escala de grises)
    #    pues esta función no procesa imágenes RGB. La funcion debe indicar un error en caso positivo
    # 2. Convertir las imágenes y mascara a tipo float 
    # 3. Normalizar todas las imagenes/mascara tipo float en el rango [0,1]
    # 4. Calcular las piramides Gaussianas de los datos convertidos a float 
    #    utilizando la funcion "gaus_piramide"
    # 5. Calcular las piramides Laplacianas de las imagenes
    #    utilizando la funcion "lapl_piramide"
    # 6. Fusionar las piramides Laplacianas de las imagenes 
    #    y la Gaussiana de la mascara con la funcion "fusionar_lapl_pyr"
    # 7. Reconstruir la piramide resultante para obtener una imagen 
    #    con la función "reconstruir_lapl_pyr"
    # 8. Tras la reconstrucción, algunos valores pueden estar fuera de rango (<0 o >255).
    #    En caso positivo, recortar a '0' o '1' según corresponda. 
    #    Además el tipo de imagen de salida debe ser float
    #
    # Argumentos de entrada:
    #   imgA: numpy array de tamaño [imagen_height, imagen_width].
    #   imgB: numpy array de tamaño [imagen_height, imagen_width].
    #   mask: numpy array de tamaño [imagen_height, imagen_width].
    #
    # Devuelve:
    #   Gpyr_imgA: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Gaussiana de la imagen A
    #   Gpyr_imgB: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Gaussiana de la imagen B
    #   Gpyr_mask: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Gaussiana de la máscara
    #   Lpyr_imgA: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Laplaciana de la imagen A
    #   Lpyr_imgB: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Laplaciana de la imagen B
    #   Lpyr_fus: lista de numpy arrays con variable tamaño con "niveles+1" elementos 
    #               correspodientes a la piramide Laplaciana de la fusion imagen A & B
    #   Lpyr_fus_rec:  numpy array de tamaño [imagen_height, imagen_width] correspondiente
    #               a la reconstruccion de la pirámide Lpyr_fus
        
    # iniciamos las variables de salida    
    Gpyr_imgA = []      # Pirámide Gaussiana imagen A
    Gpyr_imgB = []      # Pirámide Gaussiana imagen B
    Gpyr_mask = []      # Pirámide Gaussiana máscara    
    Lpyr_imgA = []      # Pirámide Laplaciana imagen A
    Lpyr_imgB = []      # Pirámide Laplaciana imagen B
    Lpyr_fus = []       # Pirámide Laplaciana fusionada
    Lpyr_fus_rec = []   # Imagen reconstruida de la pirámide Laplaciana fusionada
  
    if len(imgA.shape) != 2 or len(imgB.shape) != 2:
        raise ValueError("ERROR - Las imágenes no son bidimensionales.")

    floatImageA = img_as_float32(imgA)
    floatImageB = img_as_float32(imgB)
    floatMask = img_as_float32(mask)

    Gpyr_imgA = gaus_piramide(floatImageA, niveles)
    Gpyr_imgB = gaus_piramide(floatImageB, niveles)
    Gpyr_mask = gaus_piramide(floatMask, niveles)

    Lpyr_imgA = lapl_piramide(Gpyr_imgA)
    Lpyr_imgB = lapl_piramide(Gpyr_imgB)

    Lpyr_fus = fusionar_lapl_pyr(Lpyr_imgA, Lpyr_imgB, Gpyr_mask)

    Lpyr_fus_rec = reconstruir_lapl_pyr(Lpyr_fus)

    Lpyr_fus_rec = np.clip(Lpyr_fus_rec, 0, 1)

    return Gpyr_imgA, Gpyr_imgB, Gpyr_mask, Lpyr_imgA, Lpyr_imgB, Lpyr_fus, Lpyr_fus_rec

if __name__ == "__main__":
    #tests las tareas
    print('Testeando las tareas de la practica.')
    if not p1_tarea1.test():
        print( 'Test tarea 1 fallado.')
        sys.exit()

    if not p1_tarea2.test():
        print( 'Test tarea 2 fallado.')
        sys.exit()

    if not p1_tarea3.test():
        print( 'Test tarea 3 fallado.')
        sys.exit()
    print( 'Todos los tests completados.')
 
    #ruta de las imagenes
    filename1 = 'img/apple1.jpg'
    filename2 = 'img/orange1.jpg'
    filemask = 'img/mask_apple1_orange1.jpg'

    # lectura de imagenes     
    imgA = io.imread(filename1)
    imgB = io.imread(filename2)
    mask = io.imread(filemask)

    # comprobación de errores
    assert imgA.shape == imgB.shape and imgA.shape == mask.shape, "Error - los tamaños de imagenes y mascara son distintos"

    # conversion a escala de grises (el resultado es float en el rango [0,1])
    imgAgray = color.rgb2gray(imgA)
    imgBgray = color.rgb2gray(imgB)
    maskgray = color.rgb2gray(mask)

    # ejecutamos la fusion sin piramides    
    fus_img = imgAgray * maskgray + (1-maskgray) * imgBgray

    # ejecutamos la fusion con piramides
    niveles = 4
    Gpyr_imgA, Gpyr_imgB, Gpyr_mask, Lpyr_imgA, Lpyr_imgB, Lpyr_fus, Lpyr_fus_rec = \
        run_fusion(imgAgray, imgBgray, maskgray, niveles)

    # mostrar imágenes resultado
    fig, ax = plt.subplots( nrows = 2, ncols = 3, figsize = (8, 5), sharex = True , sharey = True )
    plt.gray()
    
    ax[0,0].imshow (imgAgray)
    ax[0,0].axis('off')
    ax[0,0].set_title ('Imagen 1')

    ax[0,1].imshow ( imgBgray )
    ax[0,1].axis ('off')
    ax[0,1].set_title ('Imagen 2')

    ax[0,2].imshow (mask)
    ax[0,2].axis('off')
    ax[0,2].set_title ('Máscara')

    ax[1,0].imshow ( fus_img )
    ax[1,0].axis ('off')
    ax[1,0].set_title ('Fusión sin piramides')

    ax[1,1].imshow ( Lpyr_fus_rec )
    ax[1,1].axis ('off')
    ax[1,1].set_title ('Fusión con piramides (' + str(niveles) + ' niveles)')

    ax[1,2].axis ('off')

    fig.tight_layout()
    plt.show ( block = False )

    # mostrar piramides Gaussianas involucradas
    Gpyr_imgA_stacked = visualizar_gaus_piramide(Gpyr_imgA)
    Gpyr_imgB_stacked = visualizar_gaus_piramide(Gpyr_imgB)
    Gpyr_mask_stacked = visualizar_gaus_piramide(Gpyr_mask)

    plt.figure(20)    
    plt.imshow(Gpyr_imgA_stacked,cmap='gray')
    plt.title('Piramide Gaussiana imagen A')

    plt.figure(2)
    plt.subplot(1, 3, 1)    
    plt.imshow(Gpyr_imgA_stacked,cmap='gray')
    plt.title('Pir Gaussiana A')
    plt.subplot(1, 3, 2)    
    plt.imshow(Gpyr_imgB_stacked)
    plt.title('Pir Gaussiana B')
    plt.subplot(1, 3, 3)    
    plt.imshow(Gpyr_mask_stacked)
    plt.title('Pir Gaussiana mask')        

    # mostrar piramides Laplacianas involucradas
    Lpyr_imgA_stacked = visualizar_lapl_piramide(Lpyr_imgA)
    Lpyr_imgB_stacked = visualizar_lapl_piramide(Lpyr_imgB)
    Lpyr_fus_stacked = visualizar_lapl_piramide(Lpyr_fus)

    plt.figure(30)    
    plt.imshow(Lpyr_imgA_stacked,cmap='gray')
    plt.title('Piramide Laplaciana imagen A')

    plt.figure(3)
    plt.subplot(1, 3, 1)    
    plt.imshow(Lpyr_imgA_stacked,cmap='gray')
    plt.title('Pir Laplaciana A')
    plt.subplot(1, 3, 2)    
    plt.imshow(Lpyr_imgB_stacked)
    plt.title('Pir Laplaciana B')
    plt.subplot(1, 3, 3)    
    plt.imshow(Lpyr_fus_stacked)
    plt.title('Pir Laplaciana fusion')    
    plt.show(block = False)

    # Tests realizados por Tomas Higuera y Pablo Hergueta
    print("Tests de fusion con piramides de diferentes niveles:")
    Gpyr_imgA, Gpyr_imgB, Gpyr_mask, Lpyr_imgA, Lpyr_imgB, Lpyr_fus, Lpyr_fus_rec8 = \
        run_fusion(imgAgray, imgBgray, maskgray, 8)

    Gpyr_imgA, Gpyr_imgB, Gpyr_mask, Lpyr_imgA, Lpyr_imgB, Lpyr_fus, Lpyr_fus_rec40 = \
        run_fusion(imgAgray, imgBgray, maskgray, 40)

    Gpyr_imgA, Gpyr_imgB, Gpyr_mask, Lpyr_imgA, Lpyr_imgB, Lpyr_fus, Lpyr_fus_rec2 = \
        run_fusion(imgAgray, imgBgray, maskgray, 2)

    # mostrar imágenes resultado
    fig, ax = plt.subplots( nrows = 2, ncols = 2, sharex = True , sharey = True )
    plt.gray()
    
    ax[0,0].imshow (Lpyr_fus_rec2)
    ax[0,0].axis('off')
    ax[0,0].set_title ('Fusion de 2 niveles')

    ax[0,1].imshow (Lpyr_fus_rec)
    ax[0,1].axis ('off')
    ax[0,1].set_title ('Fusion de 4 niveles')

    ax[1,0].imshow (Lpyr_fus_rec8)
    ax[1,0].axis('off')
    ax[1,0].set_title ('Fusion de 8 niveles')

    ax[1,1].imshow (Lpyr_fus_rec40)
    ax[1,1].axis ('off')
    ax[1,1].set_title ('Fusion de 40 niveles')

    fig.tight_layout()
    plt.show()





