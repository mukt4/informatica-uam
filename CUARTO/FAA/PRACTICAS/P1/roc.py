# Practica realizada por Tomas Higuera Viso y Alejandro Naranjo Jimenez

import numpy as np
import Clasificador
import EstrategiaParticionado
from Datos import Datos
from sklearn.metrics import confusion_matrix
import matplotlib.pyplot as plt


def roc_valsimple(dataset): 

  # Sin la correccion de laplace
  estrategia = EstrategiaParticionado.ValidacionSimple(5, 65)
  clasificador_nb = Clasificador.ClasificadorNaiveBayes(False)
  prediccion, tests = predicciones(estrategia,dataset,clasificador_nb)
  roc_matrix, result, x_axis, y_axis  = roc(tests, prediccion)
  draw_graphic(x_axis, y_axis, "Validación simple sin laplace")

  # Con la correccion de laplace
  estrategia_lp = EstrategiaParticionado.ValidacionSimple(5, 65)
  clasificador_nb_lp = Clasificador.ClasificadorNaiveBayes(True)
  prediccion_lp, tests_lp = predicciones(estrategia_lp,dataset,clasificador_nb_lp)
  roc_matrix_lp, result_lp, x_axis_lp, y_axis_lp  = roc(tests_lp, prediccion_lp)
  draw_graphic(x_axis_lp, y_axis_lp, "Validación simple con laplace")

  return roc_matrix,roc_matrix_lp

  

def draw_graphic(x, y, name):
  plt.figure()
  plt.xlabel("FPR", fontsize = 20)
  plt.ylabel("TPR", fontsize = 20)
  plt.title(name, fontsize = 20)
  plt.scatter(x,y)
  plt.show()


def roc(datos, pred):

  num = len(pred)
  roc = np.zeros((2,2))
  result = []
  x_axis = []
  y_axis = []

  for i in range(num):
    aux = confusion_matrix(datos[i][:,-1], pred[i])

    if aux.shape[0] == 2:
      TP= aux[0][0]
      FP= aux[0][1]
      FN= aux[1][0]
      TN= aux[1][1]

      if (TP + FN) == 0:
        continue

      TPR = round(TP / (TP + FN),8)
      FNR = round(FN / (TP + FN),8)

      if (FP + TN) == 0:
        continue
      
      FPR = round(FP / (FP + TN),8)
      TNR = round(TN / (FP + TN),8)

      data =[TPR, FNR, FPR, TNR]
      result.append(data)
      #roc.append(aux)
      roc = np.add(roc,aux)
      x_axis.append(FPR)
      y_axis.append(TPR)
  
  return roc , result, sorted(x_axis), sorted(y_axis)
    
  


def predicciones(particionado,dataset,clasificador,seed=None):
       
    # Creamos las particiones siguiendo la estrategia llamando a particionado.creaParticiones
    # - Para validacion cruzada: en el bucle hasta nv entrenamos el clasificador con la particion de train i
    # y obtenemos el error en la particion de test i
    # - Para validacion simple (hold-out): entrenamos el clasificador con la particion de train
    # y obtenemos el error en la particion test
    particionado.creaParticiones(dataset, seed)
    predicciones = []
    tests = []
    for particion in particionado.particiones:
      train = dataset.extraeDatos(particion.indicesTrain)
      test = dataset.extraeDatos(particion.indicesTest)
      tests.append(test)
      clasificador.entrenamiento(train, dataset.nominalAtributos,dataset.diccionarios)
      prediccion= clasificador.clasifica(test, dataset.nominalAtributos,dataset.diccionarios)
      predicciones.append(prediccion)

    return predicciones, tests


def roc_valcruzada(dataset): 
  
  # Sin la correccion de laplace
  estrategia = EstrategiaParticionado.ValidacionCruzada(10)
  clasificador_nb = Clasificador.ClasificadorNaiveBayes(False)
  prediccion, tests = predicciones(estrategia,dataset,clasificador_nb)
  roc_matrix, result, x_axis, y_axis  = roc(tests, prediccion)
  draw_graphic(x_axis, y_axis, "Validación cruzada sin laplace")

  # Con la correccion de laplace
  estrategia_lp = EstrategiaParticionado.ValidacionCruzada(10)
  clasificador_nb_lp = Clasificador.ClasificadorNaiveBayes(True)
  prediccion_lp, tests_lp = predicciones(estrategia_lp,dataset,clasificador_nb_lp)
  roc_matrix_lp, result_lp, x_axis_lp, y_axis_lp  = roc(tests_lp, prediccion_lp)
  draw_graphic(x_axis_lp, y_axis_lp, "Validación cruzada con laplace")

  return roc_matrix, roc_matrix_lp