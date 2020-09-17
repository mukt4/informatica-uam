# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 1: Fusion de imagenes mediante piramides
# Tarea 1: metodos reduce y expand
import numpy as np
import scipy.signal
import matplotlib.pyplot as plt

from p1_utils import PRECISION,check_errors_variables
from skimage import io, color, img_as_float32


def generar_kernel_suavizado(a):
    # Esta funcion devuelve un kernel de tamaño 5x5 con parametro a
    #
    # Argumentos de entrada:
    #   a: valor de tipo double o float.  
    # Devuelve:
    #   numpy array de tamaño [5, 5] (output).

    w_1d = np.array([0.25 - a/2.0, 0.25, a, 0.25, 0.25 - a/2.0])
    return np.outer(w_1d, w_1d)

def reduce(imagen):
    # Esta funcion implementa la operacion "reduce" sobre 
    # una imagen, definida en piramides paso-bajo.
    #
    # Debe realizar las siguientes operaciones:
    # 1. Crear un kernel de suavizado con a = 0.4 
    # 2. Convolucionar la imagen con este kernel 
    #   utilizando la funcion scipy.signal.convolve2d
    # 3. Muestrear por 2 el resultado de la convolucion 
    #   (i.e. coger una de cada dos muestras en ambas direcciones 
    #    empezando por la primera posicion del array)
    #
    # Argumentos de entrada:
    #   imagen: numpy array de tamaño [imagen_height, imagen_width].
    #
    # Devuelve:
    #   output: numpy array de tamaño [imagen_height/2, imagen_width/2] (output).
    #
    # NOTAS: si imagen_height/2 o imagen_width/2 no son numeros enteros, 
    #        entonces se redondea al entero mas cercano por abajo 
    #        Por ejemplo, si la imagen es 5x7, la salida sera 3x4    
    output = [] # iniciamos la variable de salida
    
    # 1. Generamos un kernel de suavizado con a = 0.4
    kernel = generar_kernel_suavizado(0.4)

    # 2. Convolucionamos la imagen con el kernel obtenido
    convolucion = scipy.signal.convolve2d(imagen, kernel, 'same')

    # 3. Muestrear por 2 el resultado de la convulucion
    # Rellenamos el array de salida con los valores de la imagen convolucionada
    output = convolucion[::2, ::2]

    return output  

def expand(imagen):
    # Esta funcion implementa la operacion "expand" sobre 
    # una imagen, definida en piramides paso-bajo.
    #
    # Debe realizar las siguientes operaciones:
    # 1. Crear una "imagen expandida" de dimension doble 
    #    comparada con la imagen de entrada 
    # 2. Copiar el contenido de imagen de entrada en la "imagen expandida".
    # 3. Crear un kernel de suavizado con a = 0.4 
    # 4. Convolucionar la imagen expandida con este kernel 
    #   utilizando la funcion  scipy.signal.convolve2d
    # 5. El resultado de expand sera la convolucion multiplicada por 4
    #   (el factor de 4 se utiliza para mantener el rango de la imagen)
    #
    # Argumentos de entrada:
    #   imagen: numpy array de tamaño [imagen_height, imagen_width].
    #
    # Devuelve:
    #   output: numpy array de tamaño [imagen_height*2, imagen_width*2].
    output = [] # iniciamos la variable de salida

    # 1. Cremaos una matriz de ceros del doble de tamanio de la imagen
    imagen_expandida = np.zeros(np.multiply(imagen.shape, 2))

    # 2. Guardamos los pixeles de la imagen en nuestra nueva matriz expandida
    imagen_expandida[::2, ::2] = imagen

    # 3. Generamos un kernel de suavizado con parametro a = 0.4
    kernel = generar_kernel_suavizado(0.4)

    # 4. Convolucianamos la imagen exapndida con el kernel y multiplicamos los valores de la matriz por 4
    output = np.multiply(scipy.signal.convolve2d(imagen_expandida, kernel, mode="same"), 4)
    
    return output

####################################################
# Esta funcion realiza una reduccion con un kernel #
# determinado.                                     #
#                                                  # 
# Entrada:                                         #
#       imagen: Numpy array que contien la imagen  #
#       a: Parametro para generar el kernel        #
#                                                  #
# Retorno: Imagen reducida                         #
####################################################
def reduce_a(imagen, a):
    output = []
    kernel = generar_kernel_suavizado(a)
    convolucion = scipy.signal.convolve2d(imagen, kernel, 'same')
    output = convolucion[::2, ::2]

    return output


####################################################
# Esta funcion realiza una expansion con un kernel #
# determinado.                                     #
#                                                  # 
# Entrada:                                         #
#       imagen: Numpy array que contien la imagen  #
#       a: Parametro para generar el kernel        #
#                                                  #
# Retorno: Imagen expandida                        #
####################################################
def expand_a(imagen, a):
    output = []
    imagen_expandida = np.zeros(np.multiply(imagen.shape, 2))
    imagen_expandida[::2, ::2] = imagen
    kernel = generar_kernel_suavizado(a)
    output = np.multiply(scipy.signal.convolve2d(imagen_expandida, kernel, mode="same"), 4)
    
    return output

####################################################
# Esta funcion calcula la energia de dos imagenes, #
# es decir, el grado de dispersion de grises entre #
# dos imaganes.                                    #
#                                                  # 
# Entrada:                                         #
#       imagen_orginal:Numpy array que contiene la #
#                      imagen original             #
#       imagen_expandida:Numpy array que contiene  #
#                        la imagen expandida       #
#                                                  #
# Retorno: Energia entre las imagenes              #
####################################################
def get_energia(imagen_original, imagen_expandida):
    diferencia = imagen_original - imagen_expandida
    sumatorio = np.sum(np.power(diferencia, 2))
    return sumatorio

def test():
    # Esta funcion verifica el correcto funcionamiento de las funciones reduce y expand, 
    # y proporciona mensajes de ayuda en caso de error.
    if __name__ == "__main__":
        print ('\tEvaluando la operacion reduce ')

    # Datos para la evaluacion de la operacion 'reduce'
    # En cada uno de los casos, cada np.array es una reduccion del previo
    reduce1 =[np.array([[   0.,    0.,    0.,    0.,    0.,    0.,    0.,    0.],
                        [   0.,    0.,    0.,    0.,    0.,    0.,    0.,    0.],
                        [   0.,    0.,  255.,  255.,  255.,  255.,    0.,    0.],
                        [   0.,    0.,  255.,  255.,  255.,  255.,    0.,    0.],
                        [   0.,    0.,    0.,    0.,    0.,    0.,    0.,    0.],
                        [   0.,    0.,    0.,    0.,    0.,    0.,    0.,    0.]]),
                np.array([[   0.64,    8.92,   12.11,    3.82],
                        [   8.29,  116.03,  157.46,   49.73],
                        [   3.82,   53.55,   72.67,   22.95]]),
                np.array([[ 12.21,  31.85],
                        [ 17.62,  45.97]]),
                np.array([[ 9.77]])] 

    reduce2 = [np.array([[ 255.,  255.,  255.,  255.,  255.,  255.,  255.],
                        [ 255.,  255.,  255.,  255.,  255.,  255.,  255.],
                        [ 255.,  255.,  125.,  125.,  125.,  255.,  255.],
                        [ 255.,  255.,  125.,  125.,  125.,  255.,  255.],
                        [   0.,    0.,    0.,    0.,    0.,    0.,    0.]]),
                np.array([[ 124.62,  173.95,  173.95,  124.62],
                        [ 165.35,  183.1 ,  183.1 ,  165.35],
                        [  51.6 ,   49.2 ,   49.2 ,   51.6 ]]),
                np.array([[  72.85,  104.71],
                        [  49.53,   68.66]]),
                np.array([[ 31.37]])] 
    
    # Evaluacion de la operacion 'reduce' para los dos casos
    for red_pyr in reduce1, reduce2:
        for imgin, true_out in zip(red_pyr[0:-1], red_pyr[1:]):        
            
            if __name__ == "__main__":
                print("\t Test imagen dimensiones" + str(imgin.shape) + "..." )

            # resultado con la funcion del usuario
            user_out = reduce(imgin)
           
            # verificacion de errores  
            if not check_errors_variables(user_out, true_out): 
                if __name__ == "__main__":
                        print("input:\n{}\n".format(imgin))
                        print("user_out:\n{}\n".format(user_out))
                        print("true_out:\n{}\n".format(true_out))
                return False
    
    if __name__ == "__main__":
        print ('\tEvaluando la operacion expand ')
    
    # Datos para la evaluacion de la operacion 'expand'
    # Tres casos: expandin[i] --> expand --> expandout[i]
    expandin = [np.array([[255]]),
                np.array([[125, 255],
                        [255,   0]]),
                np.array([[ 255.,    0.,  125.,  125.,  125.],
                        [ 255.,    0.,  125.,  125.,  125.],
                        [  50.,   50.,   50.,   50.,   50.]])] 

    expandout =[np.array([[ 163.2 ,  102.  ],
                        [ 102.  ,   63.75]]),
                np.array([[ 120.8 ,  164.75,  175.75,  102.  ],
                        [ 164.75,  158.75,  121.  ,   63.75],
                        [ 175.75,  121.  ,   42.05,   12.75],
                        [ 102.  ,   63.75,   12.75,    0.  ]]),
                np.array([[ 183.6, 114.75, 34.2, 56.25, 101.25, 112.5, 112.5,112.5, 101.25,  56.25],
                        [ 204. ,  127.5,  38.,  62.5,  112.5,  125.,  125., 125.,  112.5,  62.5 ],
                        [ 188.1, 119.75, 39.2, 61.25, 106.25, 117.5, 117.5,117.5, 105.75,  58.75],
                        [ 124.5,  88.75, 44. , 56.25,  81.25,  87.5,  87.5, 87.5,  78.75,  43.75],
                        [  56.4,  52.75, 43.8, 46.25,  51.25,  52.5,  52.5, 52.5,  47.25,  26.25],
                        [  22.5,    25.,  25.,   25.,    25.,   25.,   25.,  25.,   22.5,  12.5 ]])]

    # Evaluacion de la operacion 'expand' para los dos casos
    for imgin, true_out in zip(expandin, expandout):
            if __name__ == "__main__":
                print("\t Test imagen dimensiones" + str(imgin.shape) + "..." )

            # resultado con la funcion del usuario
            user_out = expand(imgin)
 
            # verificacion de errores  
            if not check_errors_variables(user_out, true_out): 
                if __name__ == "__main__":
                        print("input:\n{}\n".format(imgin))
                        print("user_out:\n{}\n".format(user_out))
                        print("true_out:\n{}\n".format(true_out))
                return False

    # Abrimos antes las imagenes y las pasamos a escala de grises
    #ruta de las imagenes
    filename1 = 'img/apple1.jpg'
    filename2 = 'img/orange1.jpg'

    # lectura de imagenes     
    imgA = io.imread(filename1)
    imgB = io.imread(filename2)

    # conversion a escala de grises (el resultado es float en el rango [0,1])
    imgAgray = color.rgb2gray(imgA)
    imgBgray = color.rgb2gray(imgB)

    # Test creado por Tomas Higuera y Pablo Hergueta
    # Este test prueba las operaciones expand y reduce utilizando valores extremos
    if __name__ == "__main__":
        print("Test que prueba las operaciones de expand y reduce con un kernel de suavizado con diferente valor de a")

    imgA_reduce1 = reduce_a(imgAgray, 0.4)
    imgA_reduce2 = reduce_a(imgAgray, 1.2)
    imgA_reduce3 = reduce_a(imgAgray, 2)

    imgA_expand1 = expand_a(imgAgray, 0.4)  
    imgA_expand2 = expand_a(imgAgray, 1.2)
    imgA_expand3 = expand_a(imgAgray, 2)  

    # Muestreo de imagenes reducidads y expandidas
    fig, ax = plt.subplots(nrows = 3, ncols = 1, sharex = True , sharey = True )
    plt.gray()
    
    ax[0].imshow(imgA_reduce1)
    ax[0].axis('off')
    ax[0].set_title('Imagen A reducida con kernel 0.4')

    ax[1].imshow(imgA_reduce2)
    ax[1].axis('off')
    ax[1].set_title('Imagen A reducida con kernel 1.2')

    ax[2].imshow(imgA_reduce3)
    ax[2].axis('off')
    ax[2].set_title('Imagen A reducida con kernel 2')

    fig.tight_layout()
    plt.show(block = False)

    fig, ax = plt.subplots(nrows = 3, ncols = 1, sharex = True , sharey = True )
    plt.gray()

    ax[0].imshow(imgA_expand1)
    ax[0].axis('off')
    ax[0].set_title('Imagen A expandida con kernel 0.4')

    ax[1].imshow(imgA_expand2)
    ax[1].axis('off')
    ax[1].set_title('Imagen A expandida con kernel 1.2')

    ax[2].imshow(imgA_expand3)
    ax[2].axis('off')
    ax[2].set_title('Imagen A expandida con kernel 2')

    # Test creado por Tomas Higuera y Pablo Hergueta
    # Este test hace las operaciones de expand y reduce con imagenes
    # Ademas calcula la energia al realizar reduce y expand en una imagen,
    # es decir, calcula los pixeles que se pierden al hacer estas operaciones
    # consecutivas
    if __name__ == '__main__':
        print("Test que prueba expand y reduce con imagenes")

    # Reducimos y expandimos la imagen A y la imagen B en escala de grises
    imgA_reduce = reduce(imgAgray)
    imgB_reduce = reduce(imgBgray)

    imgA_expand = expand(imgAgray)
    imgB_expand = expand(imgBgray)

    # Muestreo de imagenes reducidads y expandidas
    fig, ax = plt.subplots(nrows = 3, ncols = 2, sharex = True , sharey = True )
    plt.gray()
    
    ax[0,0].imshow(imgAgray)
    ax[0,0].axis('off')
    ax[0,0].set_title('Imagen A original')

    ax[0,1].imshow(imgBgray)
    ax[0,1].axis('off')
    ax[0,1].set_title('Imagen B original')

    ax[1,0].imshow(imgA_reduce)
    ax[1,0].axis('off')
    ax[1,0].set_title('Imagen A reducida')

    ax[1,1].imshow(imgB_reduce)
    ax[1,1].axis('off')
    ax[1,1].set_title('Imagen B reducida')

    ax[2,0].imshow(imgA_expand)
    ax[2,0].axis('off')
    ax[2,0].set_title('Imagen A expandida')

    ax[2,1].imshow(imgB_expand)
    ax[2,1].axis('off')
    ax[2,1].set_title('Imagen B expandida')

    fig.tight_layout()
    plt.show(block = False)

    # Compatacion de immagen original e imagen expandida despues de una reduccion
    fig, ax = plt.subplots(nrows = 3, ncols = 1, sharex = True, sharey = True)

    ax[0].imshow(imgAgray)
    ax[0].axis('off')
    ax[0].set_title('Imagen A original')

    # Reduccion de la imagen expandida
    imgA_red_exp = reduce(imgA_expand)

    ax[1].imshow(imgA_red_exp)
    ax[1].axis('off')
    ax[1].set_title('Imagen A reducida despues de ser expandida')

    # Expansion de la imagen reducida
    imgA_exp_red = expand(imgA_reduce)

    ax[2].imshow(imgA_exp_red)
    ax[2].axis('off')
    ax[2].set_title('Imagen A expandida despues de ser reducida')

    print("Energia(Grado de dispersion de grises de la imagen) = " + str(get_energia(imgAgray, imgA_exp_red)))

    plt.show()
    
    # no errores detectados
    return True  

if __name__ == "__main__":
    print("Practica 1 - Tarea 1\n" +
            "Realizando tests para las funciones de la Tarea 1\n" +
            "Las funciones seran correctas si los resultados obtenidos\n" +
            "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")    
    np.set_printoptions(precision=PRECISION)

    # ejecucion de la funcion test y mostrar resultado de los tests por pantalla
    print("Tests completados = " + str(test())) 