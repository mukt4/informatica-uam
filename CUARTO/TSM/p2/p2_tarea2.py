# Tratamiento de Señales Visuales @ EPS-UAM
# Practica 2: Extracción de características. Puntos de Interés.
# Parte 2: Descripcion de puntos de interés mediante histogramas

# librerias y paquetes por defecto
import numpy as np
import scipy
from skimage.feature import corner_peaks
from p2_utils import PRECISION,cls,test_p2_tarea2
from skimage import img_as_float64
from p2_tarea1 import detectar_puntos_interes_harris

# Incluya aqui las librerias que necesite en su codigo
# ...

def descripcion_puntos_interes(imagen, coords_esquinas, vtam = 16, nbins = 16, tipoDesc='hist'):
    # Esta funcion describe puntos de interes de una imagen mediante histogramas,
    # analizando vecindarios de tamaño "vtam"x"vtam" centrados en cada punto de interes
    #   
    # La descripcion obtenida depende del parametro 'tipoDesc':
    #   - 'hist': histograma normalizado de valores de gris 
    #
    # En el caso de que existan puntos de interes en los bordes de la imagen, el descriptor no
    # se calcula y el punto de interes se elimina de la lista <new_coords_esquinas> que devuelve
    # esta funcion. Esta lista indica los puntos de interes para los cuales existe descriptor.
    #
    # Argumentos de entrada:
    #   - imagen: numpy array de tamaño [imagen_height, imagen_width].        
    #   - coords_esquinas: numpy array de tamaño [num_puntos_interes, 2] con las coordenadas 
    #                      de los puntos de interes detectados en la imagen. Tipo int64
    #   - vtam: valor de tipo entero que indica el tamaño del vecindario a considerar para
    #           calcular el descriptor correspondiente.
    #   - nbins: valor de tipo entero que indica el numero de niveles que tiene el histograma 
    #           para calcular el descriptor correspondiente.
    #   - tipoDesc: cadena de caracteres que indica el tipo de descriptor calculado
    #
    # Argumentos de salida
    #   - descriptores: numpy array de tamaño [num_puntos_interes, nbins] con los descriptores 
    #                   de cada punto de interes (i.e. histograma de niveles de gris)
    #   - new_coords_esquinas: numpy array de tamaño [num_puntos_interes, 2], solamente con las coordenadas 
    #                      de los puntos de interes descritos. Tipo int64
    #
    # NOTA: no modificar los valores por defecto de las variables de entrada vtam y nbins, 
    #       pues se utilizan para verificar el correcto funciomaniento de esta funcion
    
    # Iniciamos variables de salida
    descriptores = []
    new_coords_esquinas = []

    # Primero convertimos la imagen a float y la normalizamos entre [0,1]
    img_float = img_as_float64(imagen)

    #  Si los descriptores tienen que ser de tipo histograma
    if tipoDesc == 'hist':
        for punto in coords_esquinas:
            # Obtenemos las coordenadas x e y de la diagonal izquierda superior
            coord_x_diag = punto[0] - int(np.floor(vtam/2))
            coord_y_diag = punto[1] - int(np.floor(vtam/2))

            if coord_x_diag < 0 or coord_y_diag < 0 or coord_y_diag + vtam > img_float.shape[1] - 1 or coord_x_diag + vtam > img_float.shape[0] - 1:
                pass
            else:
                # Añadimos el punto de interés descrito
                new_coords_esquinas.append(punto)
                # Obtenemos la matriz de puntos de interes
                matriz_interes = img_float[coord_x_diag : coord_x_diag + vtam + 1, coord_y_diag : coord_y_diag + vtam + 1]
                # Añadimos el histograma al conjunto de descriptores
                descriptores.append(np.histogram(matriz_interes, nbins, (0, 1))[0])
    # Si los descriptores tienen que ser de tipo magnitud-orientacion
    elif tipoDesc == 'mag-ori':
        # Para poder realizar este tipo de descriptores primero es necesario que hayemos el modulo del gradiente y la orientacion del mismo
        # Para ello realizaremos tanto el modulo como la orientacion de la imagen entera, para luego
        # poder obtener los diferentes puntos en funcion del tamano de la ventana y la posicion del descriptores
        # Lo primero que hacemos es obtener las derivadas parciales, es decir, el gradiente de la imagen
        dx = scipy.ndimage.sobel(img_float, axis=0, mode='constant')
        dy = scipy.ndimage.sobel(img_float, axis=1, mode='constant')
        # Hallamos la magnitud del gradiente haciendo la raiz de la suma de los cuadrados de las derivadas parciales
        magnitud = np.sqrt(dx**2  + dy**2)
        # Hallamos la orientacion de los gradientes haciendo el arctg(b/a)
        orientacion_rad = np.arctan2(dy, dx)
        # Pasamos la orientacion a grados
        orientacion_deg = np.rad2deg(orientacion_rad)
        # Una vez hemos obtenido la orientacion la transsformamos al formato correcto, es decir, en el intervalo 0-360 grados
        orientacion_deg[orientacion_deg < 0] = orientacion_deg[orientacion_deg < 0] + 360
        # Una vez hemos obtenido orientacion y magnitud del gradiente pasamos a realizar el histograma
        # El histograma debera tener tamano nbins, por lo que deberemos dividir los 360 grados en nbins
        # Usamos la funcion linespace para dividir los 360 grados
        division_grados = np.linspace(0, 360, nbins + 1)
        total = 0
        # Pasamos ahora a tratar los puntos caracteristicos
        for punto in coords_esquinas:
            # Inicializamos el histograma magnitud-orientacion del punto
            histograma_mag_ori = []
            # Obtenemos las coordenadas x e y de la diagonal izquierda superior
            coord_x_diag = punto[0] - int(np.floor(vtam/2))
            coord_y_diag = punto[1] - int(np.floor(vtam/2))
            # Si las coordinadas no cumplen la siguiente condicion significara que la ventana de interes se sale de la imagen
            if coord_x_diag < 0 or coord_y_diag < 0 or coord_y_diag + vtam > img_float.shape[1] - 1 or coord_x_diag + vtam > img_float.shape[0] - 1:
                pass
            # Si cumple la condicion podemos tratar la ventana
            else:
                # Calculamos la ventana de interes de orientacion y de magnitud de cada punto de interes
                matriz_interes_magnitud = magnitud[coord_x_diag : coord_x_diag + vtam + 1, coord_y_diag : coord_y_diag + vtam + 1]
                matriz_interes_orientacion = orientacion_deg[coord_x_diag : coord_x_diag + vtam + 1, coord_y_diag : coord_y_diag + vtam + 1]
                # Recorremos los grados divididos para rellenar el histograma
                # Empezamos en el valor 1 de la lista de grados, ya que el 0 pertenece a este intervalo
                for k, grado in enumerate(division_grados[1:]):
                    # Guardamos una variable suma a 0 para guardar los valores en el histograma
                    suma = 0
                    # Recorremos las orientaciones
                    for i, orientaciones in enumerate(matriz_interes_orientacion):
                        for j, orientacion in enumerate(orientaciones):
                            # Si la orientacion esta en el intervalo correcto
                            if orientacion < grado and orientacion >= division_grados[k]:
                                # Creamos una lista que almacenara los valores que vamos a sumar
                                suma += matriz_interes_magnitud[i][j]
                    # Aniadimos el valor al histograma
                    # El resultado que obtengamos lo redondearemos dos decimales
                    histograma_mag_ori.append(round(suma, 2))
                # Guardamos las coordenadas de la esquina
                new_coords_esquinas.append(punto)
                # Guardamos el descriptor
                descriptores.append(histograma_mag_ori)
    # Si no se trata de ninguno de los descriptores esperados devolvemos tanto corrspondencias como las coordenadas
    # de las esquinas ecomo arrays vacios
    else:
        descriptores = []
        new_coords_esquinas = []

    return np.array(descriptores), np.array(new_coords_esquinas)

if __name__ == "__main__":
    cls()
    print("TSV - Practica 2 - Parte 2\n" +
            "Realizando tests para las funciones de la parte 2\n" +
            "Las funciones seran correctas si los resultados obtenidos\n" +
            "tienen una tolerancia de dos decimales con respecto a la salida correcta.\n")    
    np.set_printoptions(precision=PRECISION)

    # ejecucion de la funcion test y mostrar resultado de los tests por pantalla
    disptime = -1
    print("Tests completados = " + str(test_p2_tarea2(disptime = disptime, tipoDesc='hist')))

