# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 1: Fusion de imagenes mediante piramides
# Tarea 2: piramide Gaussiana y piramide laplaciana
import numpy as np

from p1_utils import PRECISION,check_errors_piramides
from p1_tarea1 import reduce, expand

def gaus_piramide(imagen, niveles):
    # Esta funcion implementa la creacion de una piramide Gaussiana
    # utilizando la operacion "reduce" sobre una imagen
    #
    # Debe realizar las siguientes operaciones:
    # - Crear un kernel de suavizado con a = 0.4 
    # - Convolucionar la imagen con este kernel 
    #   utilizando la funcion 'convolucion2d' de la parte 1
    # - Muestrear por 2 el resultado de la convolucion 
    #   (i.e. coger una de cada dos muestras en ambas direcciones 
    #    empezando por la primera posicion del array)
    #
    # Argumentos de entrada:
    #   imagen: numpy array de tamaño [imagen_height, imagen_width].
    #   niveles: entero positivo que especifica el numero de veces que
    #           se aplica la operacion 'reduce'. 
    #           Si niveles=0 entonces se debe devolver una lista con la imagen de entrada
    #           Si niveles=1 entonces se debe realizar una operacion de reduccion    
    #           
    #
    # Devuelve:
    #   gauss_pyr: lista de numpy arrays con variable tamaño con "niveles+1" elementos.
    #       output[0] es la imagen de entrada
    #       output[i] es el nivel i de la piramide        
    gaus_pyr = []  # iniciamos la variable de salida

    # Bucle en el que aplicamos la operacion reduce nivel a nivel
    # Almacenamos la imagen en la posicion 0 del array
    gaus_pyr.append(imagen)
    for nivel in range(niveles):
        gaus_pyr.append(reduce(gaus_pyr[nivel]))

    return gaus_pyr

def lapl_piramide(gaus_pyr):
    # Esta funcion implementa la creacion de una piramide Laplaciana
    # a partir de una piramide Gaussiana    
    #
    # Esta funcion debe considerar lo siguiente:
    # - Cada nivel 'k' de la piramide Laplaciana se debe obtener como 
    #   la resta entre el nivel 'k' de la piramide Gaussiana y la 
    #   expansion (operacion expand) del nivel 'k+1' de la piramide Gaussiana        
    # - El numero de niveles de la piramide laplaciana viene determinado por 
    #   numero de niveles de la piramide Gaussiana
    # - El ultimo elemento (imagen) de la piramide Laplaciana es identico 
    #   al ultimo elemento (imagen) de la piramide Gaussiana
    #
    # Argumentos de entrada:
    #   gauss_pyr: lista de numpy arrays creada con la funcion 'gauss_piramide'.               
    #
    # Devuelve:
    #   lapla_pyr: lista de numpy arrays con variable tamaño con "niveles+1" elementos.    
    #       lapla_pyr[i] es el nivel i de la piramide que contiene bordes
    #       lapla_pyr[niveles] es una imagen (RGB o escala de grises)
    #
    # NOTA: algunas veces, la operacion 'expand' devuelve una imagen de tamaño mayor 
    # que el esperado. Entonces no coinciden las dimensiones de la imagen expandida 
    #   del nivel k+1 y las dimensiones del nivel k. Verifique si ocurre esta 
    #   situacion y en caso afirmativo, elimine los ultimos elementos de la 
    #   imagen expandida hasta coincidir las dimensiones del nivel k
    #   Por ejemplo, si el nivel tiene tamaño 5x7, tras aplicar 'reduce' y 'expand' 
    #   obtendremos una imagen de tamaño 6x8. En este caso, elimine la 6 fila y 8 
    #   columna para obtener una imagen de tamaño 5x7 donde pueda aplicar la resta
    lapl_pyr = [] # iniciamos la variable de salida    

    for nivel in range(len(gaus_pyr) - 1):
        try:
            lapl_pyr.append(gaus_pyr[nivel] - expand(gaus_pyr[nivel + 1]))
        except ValueError:
            exp = expand(gaus_pyr[nivel + 1])[:gaus_pyr[nivel].shape[0], :gaus_pyr[nivel].shape[1]]
            lapl_pyr.append(gaus_pyr[nivel] - exp)

    lapl_pyr.append(gaus_pyr[nivel + 1])

    return lapl_pyr

def test():
    # Esta funcion verifica el correcto funcionamiento de las funciones 
    # para crear piramides Gaussianas y piramides Laplacianas
    # y proporciona mensajes de ayuda en caso de error.

    if __name__ == "__main__":
        print ('\tEvaluando la creacion de piramide Gaussiana ')

    # Datos para la evaluacion de piramide Gaussiana (dos piramides Gaussianas)    
    gauss_pyr1 =[np.array([[   0.,    0.,    0.,    0.,    0.,    0.,    0.,    0.],
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

    gauss_pyr2 = [np.array([[ 255.,  255.,  255.,  255.,  255.,  255.,  255.],
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

    # Evaluacion de la operacion 'gauss_piramide' para los dos casos
    for pyr in gauss_pyr1, gauss_pyr2:  
        if __name__ == "__main__":  
            print("\tTest piramide Gaussiana para imagen con dimensiones" + str(pyr[0].shape) + "..." )

        # resultado con la funcion del usuario            
        user_out = gaus_piramide(pyr[0], 3)
    
        # verificacion de errores  
        true_out = pyr
        if not check_errors_piramides(user_out, true_out): 
            return False
   
    if __name__ == "__main__":
        print ('\tEvaluando la creacion de piramide Laplaciana ')

    # Datos para la evaluacion de piramide Laplaciana (dos piramides Laplaciana)    
    lapl_pyr1 =[np.array([[  -2.95,  -10.04,  -17.67,  -22.09,  -23.02,  -16.73,   -8.97,   -4.01],
                        [  -9.82,  -33.47,  -58.9 ,  -73.63,  -76.75,  -55.78,  -29.9 ,  -13.39],
                        [ -15.57,  -53.07,  161.59,  138.24,  133.29,  166.55,  -47.41,  -21.23],
                        [ -13.32,  -45.42,  175.06,  155.07,  150.83,  179.3 ,  -40.58,  -18.17],
                        [  -8.55,  -29.16,  -51.33,  -64.16,  -66.88,  -48.61,  -26.05,  -11.67],
                        [  -4.21,  -14.34,  -25.24,  -31.55,  -32.89,  -23.91,  -12.81,   -5.74]]),
              np.array([[ -11.59,  -11.88,  -13.1 ,  -11.22],
                        [  -7.53,   89.12,  124.84,   30.27],
                        [ -12.43,   25.91,   39.17,    2.97]]),
              np.array([[  5.96,  27.94],
                        [ 13.71,  43.53]]),
              np.array([[ 9.77]])] 

    lapl_pyr2 =[np.array([[ 146.27,  118.15,  101.65,   97.53,  101.65,  118.15,  146.27],
                        [ 121.16,   93.25,   79.83,   76.48,   79.83,   93.25,  121.16],
                        [ 118.2 ,   95.65,  -41.91,  -43.79,  -41.91,   95.65,  118.2 ],
                        [ 156.61,  142.69,    9.62,    8.85,    9.62,  142.69,  156.6 ],
                        [ -52.02,  -57.74,  -57.68,  -57.67,  -57.68,  -57.74,  -52.02]]),
              np.array([[  64.97,   97.02,   95.12,   79.3 ],
                        [ 107.73,  109.16,  107.63,  122.01],
                        [   7.53,   -6.95,   -7.81,   18.9 ]]),
              np.array([[ 52.77,  92.16],
                        [ 36.98,  60.82]]),
              np.array([[ 31.37]])] 
   
    # Evaluacion de la operacion 'lapla_piramide' para los dos casos
    for gaus_pyr, lapl_pyr in zip((gauss_pyr1, gauss_pyr2), (lapl_pyr1, lapl_pyr2)):
        if __name__ == "__main__":
            print("\tTest piramide Laplaciana para imagen con dimensiones" + str(gaus_pyr[0].shape) + "..." )

        # resultado con la funcion del usuario            
        user_out = lapl_piramide(gaus_pyr)

        # verificacion de errores  
        true_out = lapl_pyr
        if not check_errors_piramides(user_out, true_out): 
            return False
    
    # no errores detectados
    return True
   
if __name__ == "__main__":
    print("Practica 1 - Tarea 2\n" +
            "Realizando tests para las funciones de la tarea 2\n" +
            "Las funciones seran correctas si los resultados obtenidos\n" +
            "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")    
    np.set_printoptions(precision=PRECISION)

    # ejecucion de la funcion test y mostrar resultado de los tests por pantalla
    print("Tests completados = " + str(test()))   