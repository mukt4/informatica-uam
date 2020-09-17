# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 1: Fusion de imagenes mediante piramides
# Tarea 3: fusion y reconstruccion de piramides laplacianas
import numpy as np

from p1_utils import PRECISION, check_errors_variables, check_errors_piramides
from p1_tarea1 import expand

def fusionar_lapl_pyr(lapl_pyr_imgA, lapl_pyr_imgB, gaus_pyr_mask):
    # Esta funcion realiza la fusion entre dos piramides laplacianas de distintas imagenes.
    #   La fusion esta determinada por una mascara de la cual se utiliza su correspondiente
    #   piramide Gaussiana para combinar las dos piramides laplacianas.
    #
    # Debe realizar las siguientes operaciones:
    # - Las piramides han de tener el mismo numero de niveles. 
    #   La funcion debe indicar un error en caso de que no sean similares
    # - Se debe obtener una nueva piramide laplaciana con el mismo numero de 'niveles' 
    #   que las piramides laplacianas
    # - Cada nivel de la nueva piramide se corresponde con una suma ponderada con la mascara
    #   de los niveles de las piramides de entrada
    #       Lpyr_out[k] = lapl_pyr_imgA[k]*gaus_pyr_mask[k] + lapl_pyr_imgB[k]*(1-gaus_pyr_mask[k])
    #
    # Argumentos de entrada:
    #   lapl_pyr_imgA: lista de numpy arrays obtenida con la funcion 'lapl_piramide' sobre una imagen img2
    #   lapl_pyr_imgB: lista de numpy arrays obtenida con la funcion 'lapl_piramide' sobre otra imagen img1
    #   gaus_pyr_mask: lista de numpy arrays obtenida con la funcion 'gaus_piramide' 
    #                  sobre una mascara para combinar ambas imagenes. 
    #                  Esta mascara y la piramide tiene valores en el rango [0,1]
    #                  Para los pixeles donde gaus_pyr_mask==1, se coge la piramide de img1
    #                  Para los pixeles donde gaus_pyr_mask==0, se coge la piramide de img2
    #    
    # Devuelve:
    #   fusion_pyr: piramide fusionada
    #       lista de numpy arrays con variable tamaño con "niveles+1" elementos.    
    #       fusion_pyr[i] es el nivel i de la piramide que contiene bordes
    #       fusion_pyr[niveles] es una imagen (RGB o escala de grises)
    fusion_pyr = [] # iniciamos la variable

    if not (len(lapl_pyr_imgA) == len(lapl_pyr_imgB) == len(gaus_pyr_mask)):
        raise TypeError("ERROR - Las pirámides han de tener el mismo número de niveles.")

    for nivel in range(len(lapl_pyr_imgA)):
        fusion_pyr.append(lapl_pyr_imgA[nivel] * gaus_pyr_mask[nivel] + lapl_pyr_imgB[nivel] * (1 - gaus_pyr_mask[nivel]))

    return fusion_pyr

def reconstruir_lapl_pyr(lapl_pyr):
    # Esta funcion reconstruye la imagen dada una piramide laplaciana.
    #
    # Debe realizar las siguientes operaciones:
    # 1. Expandir el nivel 'k' de la piramide
    # 2. Sumar la imagen expandida con el nivel 'k-1'
    # 3. Repetir la operación de manera iterativa
    #
    # Argumentos de entrada:
    #   lapl_pyr: lista de numpy arrays obtenida con la funcion 'lapl_piramide' sobre una imagen img
    #    
    # Devuelve:
    #   output: numpy array con dimensiones iguales al primer nivel de la piramide lapl_pyr[0]
    #
    # NOTA: algunas veces, la operacion 'expand' devuelve una imagen de tamaño mayor 
    # que el esperado. Entonces no coinciden las dimensiones de la imagen expandida 
    #   del nivel k+1 y las dimensiones del nivel k. Verifique si ocurre esta 
    #   situacion y en caso afirmativo, elimine los ultimos elementos de la 
    #   imagen expandida hasta coincidir las dimensiones del nivel k
    #   Por ejemplo, si el nivel tiene tamaño 5x7, tras aplicar 'reduce' y 'expand' 
    #   obtendremos una imagen de tamaño 6x8. En este caso, elimine la 6 fila y 8 
    #   columna para obtener una imagen de tamaño 5x7 donde pueda aplicar la resta
    output = lapl_pyr[-1]  # iniciamos la variable de salida

    for nivel in reversed(range(len(lapl_pyr) - 1)):
        try:
            output = expand(output) + lapl_pyr[nivel]
        except ValueError:
            exp = expand(output)[:lapl_pyr[nivel].shape[0], :lapl_pyr[nivel].shape[1]]
            output = exp + lapl_pyr[nivel]

    return output

def test():
    # Esta funcion verifica el correcto funcionamiento de las funciones 
    # 'fusionar_lapl_pyr' y 'reconstruir_lapl_pyr'
    if __name__ == "__main__":
        print ('\tEvaluando la fusion de piramides Laplacianas ')

    # Datos para la evaluacion de fusion y reconstruccion de piramides Laplacianas (dos piramides)    
    lapl_pyr11 =[np.array([[ 0.,  0.,  0.,  0.],
                          [ 0.,  0.,  0.,  0.],
                          [ 0.,  0.,  0.,  0.]]),
                np.array([[ 0.,  0.],
                          [ 0.,  0.]])] 
    lapl_pyr12 =[np.array([[ 149.77,  122.46,  121.66,  178.69],
                            [ 138.08,  107.74,  106.84,  170.21],
                            [ 149.77,  122.46,  121.66,  178.69]]),
                np.array([[ 124.95,  169.58],
                            [ 124.95,  169.57]])] 
    lapl_pyr21 =[np.array([[ 149. ,  118.4,   99.2,   94.3,   99.2,  118.4,  149. ],
                            [ 137.2,  103.3,   81.9,   76.5,   81.9,  103.3,  137.2],
                            [ 148.1,  117.4,   97.9,   93.1,   97.9,  117.4,  148.1],
                            [ -63.1,  -81.3,  -92.8,  -95.6,  -92.8,  -81.3,  -63.1],
                            [ -18.5,  -23.8,  -27.2,  -28. ,  -27.2,  -23.8,  -18.5]]),
                np.array([[  70.4,  107.1,  104.5,   82.3],
                            [  76.7,  115.4,  113.1,   87.3],
                            [ -23.3,  -29.4,  -31. ,  -16.3]]),
                np.array([[  67.7,  100.3],
                            [  34. ,   50.4]])] 
    lapl_pyr22 =[np.array([[  -5. ,  -25.2,  -56.4,  149.8,  110.3,  116.2,  144.8],
                            [  -6.5,  -32.5,  -72.6,  119.5,   68.6,   76.2,  113. ],
                            [  -7.2,  -36. ,  -80.3,  105.2,   48.9,   57.2,   98. ],
                            [  -6.5,  -32.5,  -72.6,  119.5,   68.6,   76.2,  113. ],
                            [  -5. ,  -25.2,  -56.4,  149.8,  110.3,  116.2,  144.8]]),
                np.array([[ -20.9,    4.8,  102.6,   84.1],
                            [ -23.2,   22.3,  167.9,  133.1],
                            [ -20.9,    4.8,  102.6,   84.1]]),
                np.array([[ 17.6,  90.8],
                            [ 17.6,  90.8]])] 
    mask_pyr1 =[np.array([[ 0.,  0.,  1.,  1.],
                        [ 0.,  0.,  1.,  1.],
                        [ 0.,  0.,  1.,  1.]]),
                np.array([[ 0.03,  0.46],
                        [ 0.03,  0.46]])] 
    mask_pyr2 = [np.array([[ 0.,  0.,  0.,  0.,  1.,  1.,  1.],
                            [ 0.,  0.,  0.,  0.,  1.,  1.,  1.],
                            [ 0.,  0.,  0.,  0.,  1.,  1.,  1.],
                            [ 0.,  0.,  0.,  0.,  1.,  1.,  1.],
                            [ 0.,  0.,  0.,  0.,  1.,  1.,  1.]]),
                np.array([[ 0. ,  0. ,  0.5,  0.5],
                            [ 0. ,  0. ,  0.7,  0.7],
                            [ 0. ,  0. ,  0.5,  0.5]]),
                np.array([[ 0. ,  0.3],
                            [ 0. ,  0.3]])] 
    out_pyr1 =[np.array([[ 149.77,  122.46,    0.  ,    0.  ],
                        [ 138.08,  107.74,    0.  ,    0.  ],
                        [ 149.77,  122.46,    0.  ,    0.  ]]),
                np.array([[ 120.58,   92.42],
                        [ 120.58,   92.42]])] 
    out_pyr2 = [np.array([[  -5. ,  -25.2,  -56.4,  149.8,   99.2,  118.4,  149. ],
                        [  -6.5,  -32.5,  -72.6,  119.5,   81.9,  103.3,  137.2],
                        [  -7.2,  -36. ,  -80.3,  105.2,   97.9,  117.4,  148.1],
                        [  -6.5,  -32.5,  -72.6,  119.5,  -92.8,  -81.3,  -63.1],
                        [  -5. ,  -25.2,  -56.4,  149.8,  -27.2,  -23.8,  -18.5]]),
                np.array([[ -20.9,    4.8,  103.5,   83.2],
                        [ -23.2,   22.3,  129.5,  101. ],
                        [ -20.9,    4.8,   35.8,   33.9]]),
                np.array([[ 17.6,  93.6],
                        [ 17.6,  78.7]])]    

    # Evaluacion de la operacion 'fusionar_lapl_pyr' para los dos casos
    for left_pyr, right_pyr, mask_pyr, out_pyr in ((lapl_pyr11, lapl_pyr12, mask_pyr1, out_pyr1), 
      (lapl_pyr21, lapl_pyr22, mask_pyr2, out_pyr2)):
        if __name__ == "__main__":
            print("\tTest fusion piramide Laplaciana con {} niveles..." .format(len(left_pyr)))

        # resultado con la funcion del usuario                    
        user_out = fusionar_lapl_pyr(left_pyr, right_pyr, mask_pyr)

        # verificacion de errores  
        true_out = out_pyr
        if not check_errors_piramides(user_out, true_out): 
            return False

    if __name__ == "__main__":
        print ('\tEvaluando la reconstruccion de piramides Laplacianas')

    # Datos (imagenes obtenidas tras fusionar las piramides Laplacianas indicadas anteriormente out_pyr1 y out_pyr2)
    outimg1 = np.array([[ 244.91,  218.31,   77.39,   41.59],
                        [ 243.79,  214.24,   85.99,   46.21],
                        [ 244.91,  218.31,   77.39,   41.59]]) 
    outimg2 = np.array([[   0.1,    0.1,   -0.1,  253.7,  241.3,  254. ,  256. ],
                        [  -0.3,   -0.5,   -2.7,  244.4,  250.3,  263.3,  263.2],
                        [  -0.6,   -1.4,   -6. ,  233.4,  267.8,  278.2,  274.6],
                        [  -0.9,   -2.1,   -8.7,  224.1,   42.2,   46.1,   37.3],
                        [  -1. ,   -2.4,   -9.6,  221.2,   61.5,   59.5,   47.5]])

    # Evaluacion de la operacion 'reconstruir_lapl_pyr' para los dos casos    
    for pyr, img in ((out_pyr1, outimg1),(out_pyr2, outimg2)):
        
        if __name__ == "__main__":
            print("\tTest reconstruccion piramide Laplaciana con {} niveles..." .format(len(pyr)))
        
        # resultado con la funcion del usuario                    
        user_out = reconstruir_lapl_pyr(pyr)
        
        # verificacion de errores  
        true_out = img
        if not check_errors_variables(user_out, true_out): 
            if __name__ == "__main__":
                print("input:\n{}\n".format(pyr))
                print("user_out:\n{}\n".format(user_out))
                print("true_out:\n{}\n".format(true_out))
            return False

    return True

if __name__ == "__main__":
    print("Practica 1 - Tarea 3\n" +
            "Realizando tests para las funciones de la tarea 3\n" +
            "Las funciones seran correctas si los resultados obtenidos\n" +
            "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")    
    np.set_printoptions(precision=PRECISION)

    # ejecucion de la funcion test y mostrar resultado de los tests por pantalla
    print("Tests completados = " + str(test())) 