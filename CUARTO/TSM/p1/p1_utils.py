import numpy as np
from cv2 import cv2

# constante para determinar la precision con la que se
# analiza la similitud entre dos variables
PRECISION = 2

def check_errors_variables(user_out, true_out):
    # Esta funcion verifica la similitud entre dos variables.
    # y proporciona mensajes de ayuda en caso de error.
    #
    # Argumentos de entrada:
    #   user_out: numpy array de tamaño [image_height, image_width] generada por el usuario.
    #   true_out: numpy array de tamaño [image_height, image_width] con el resultado correcto.
    # Devuelve:
    #   True si todas las verificaciones son correctas
    #   False si alguna verificación no es correcta.

    # verificacion de tipo
    if not type(user_out) == type(true_out):        
        print("Error! - Resultado tiene tipo {} (se espera tipo {}).".format(type(user_out), type(true_out)))
        return False

    # verificacion de dimensiones
    if not user_out.shape == true_out.shape:
        print("Error! - Resultado tiene dimensiones {} (se espera dimensiones {}).".format(user_out.shape, true_out.shape))          
        return False

    # verificacion de dtype
    if not user_out.dtype == true_out.dtype:
        print("Error! - Resultado tiene dtype {} (se espera dtype {}).".format(user_out.dtype, true_out.dtype))                    
        return False

    # verificacion de valores
    #if not np.array_equal(user_out,true_out):              # debe ser identicos
    if not np.allclose(user_out, true_out, atol = 10^PRECISION):    #tolerancia de dos decimales
        print("Error! - Resultado tiene valores distintos a los esperados.")                    
        return False
        
    return True

def check_errors_piramides(user_out, true_out):
    # Esta funcion verifica la similitud entre dos piramides.
    # y proporciona mensajes de ayuda en caso de error.
    #
    # Argumentos de entrada:
    #   user_out: lista con numpy arrays generada por el usuario.
    #   true_out: lista con numpy array con el resultado correcto.
    # Devuelve:
    #   True si todas las verificaciones son correctas
    #   False si alguna verificación no es correcta.

    # verificacion de tipo
    if not type(user_out) == type(true_out):        
        print("Error! - Resultado Piramide tiene tipo {} (se espera tipo {}).".format(type(user_out), type(true_out)))
        return False

    # verificacion de dimensiones
    if not len(user_out) == len(true_out):
        print("Error! - Resultado Piramide tiene longitud {} (se espera longitud {}).".format(len(user_out), len(true_out)))          
        return False

    # verificacion por cada nivel de la piramide
    for i, (user_layer, true_layer) in enumerate(zip(user_out, true_out)):
      # (nivel) verificacion de tipo
      if not type(user_layer) == type(true_layer):
        print("Error! - Nivel {} de la piramide tiene tipo {} (se espera tipo {}).".format(i,type(user_layer), type(true_layer)))
        return False

      # (nivel) verificacion de las dimensiones
      if not user_layer.shape == true_layer.shape:
        print("Error! - Nivel {} de la piramide tiene dimensiones {} (se espera dimensiones {}).".format(i,user_layer.shape, true_layer.shape))
        return False

      # (nivel) verificacion del data type
      if not user_layer.dtype == true_layer.dtype:
        print("Error! - Nivel {} de la piramide tiene dtype {} (se espera dtype {}).".format(i,user_layer.dtype, true_layer.dtype))
        return False

      # (nivel) verificacion de los valores
      if not np.allclose(user_layer, true_layer, atol = 10^PRECISION):    #tolerancia de dos decimales
        print("Error! - Nivel {} de la piramide tiene valores distintos a los esperados.".format(i))        
        return False
        
    return True

def visualizar_gaus_piramide(gauss_pyr):
    # Esta funcion visualiza una piramide Gaussiana 
    # y crea una sola imagen con todos los niveles
    #
    # Argumentos de entrada:
    #   gauss_pyr: lista de numpy arrays creada con la funcion 'gauss_piramide'.     
    # Devuelve:
    #   output: imagen con la visualizacion de la piramide (tipo uint8)
    
    height = gauss_pyr[0].shape[0]
    width = gauss_pyr[0].shape[1]

    output = np.zeros((height*len(gauss_pyr), width), dtype = float)

    for idx, layer in enumerate(gauss_pyr):
        if layer.max() <= 1:
            layer = layer.copy() * 255

        output[(idx*height):((idx+1)*height),:] = cv2.resize(layer, (width, height), interpolation=cv2.INTER_AREA)

    return output.astype(np.uint8)

def visualizar_lapl_piramide(lapl_pyr):
    # Esta funcion visualiza una piramide Laplaciana 
    # y crea una sola imagen con todos los niveles
    #
    # Argumentos de entrada:
    #   lapl_pyr: lista de numpy arrays creada con la funcion 'lapl_piramide'.     
    # Devuelve:
    #   output: imagen con la visualizacion de la piramide (tipo uint8)
    height = lapl_pyr[0].shape[0]
    width = lapl_pyr[0].shape[1]

    output = np.zeros((height*len(lapl_pyr), width), dtype = np.uint8)

    for idx, layer in enumerate(lapl_pyr[:-1]):
        patch = cv2.resize(layer, (width, height), interpolation=cv2.INTER_AREA).astype(float)
        
        # escalar el patch al rango [0,255]
        patch = 128 + 127*patch/(np.abs(patch).max())

        output[(idx*height):((idx+1)*height),:] = patch.astype(np.uint8)

    #incluimos el ultimo nivel ultimo nivel
    patch = cv2.resize(lapl_pyr[-1], (width, height), interpolation=cv2.INTER_AREA)
    output[((len(lapl_pyr)-1)*height):(len(lapl_pyr)*height),:] = 255*patch

    return output