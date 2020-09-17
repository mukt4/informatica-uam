# -*- coding: utf-8 -*-
from Datos import Datos
from plotModel import plotModel
from IPython.display import *
import EstrategiaParticionado 
import Clasificador
import numpy as np
import matplotlib.pyplot as plt

def exe_KNN(dataset=None,estrategia=None,k_to_exe=3,norm = True):
  
  table = "<table><tr> <th>k</th> <th>Sin Normalizar</th>"
  if norm:
    table += "<th>Normalizando</th>"
  table += "</tr>"

  for i in k_to_exe:
    
    clasificador_sn=Clasificador.ClasificadorVecinosProximos(i,False)
    errores_sn=clasificador_sn.validacion(estrategia,dataset,clasificador_sn)

    table += "<tr> <td>" + str(i)
    table += "</td><td>%.4f  ±%4.f</td>" % (np.mean(errores_sn), np.std(errores_sn))

    if norm:
      clasificador_n=Clasificador.ClasificadorVecinosProximos(i,True)
      errores_n=clasificador_n.validacion(estrategia,dataset,clasificador_n)

      table += "<td>%.4f  ±%4.f</td> </tr>" % (np.mean(errores_n), np.std(errores_n))
    else:
      table += "</tr>"

  table += "</table>"

  return table


def exe_KNN_plot(dataset=None,estrategia=None,k=3,data="none",norm=True):

  title = "KNN con " + data + " y k=" + str(k) + "  "

  if norm:
    clasificador=Clasificador.ClasificadorVecinosProximos(k,True)
    title += "normalizado"
  else:
    clasificador=Clasificador.ClasificadorVecinosProximos(k,False)

  errores=clasificador.validacion(estrategia,dataset,clasificador)

    #índices de entrenamiento de la última partición
  ii = estrategia.particiones[-1].indicesTrain

  plotModel(dataset.datos[ii,0], dataset.datos[ii,1], dataset.datos[ii,-1] != 0, clasificador,title, dataset.diccionarios)
  plt.plot(dataset.datos[dataset.datos[:,-1]==0,0], dataset.datos[dataset.datos[:,-1]==0, 1], 'ro')
  plt.plot(dataset.datos[dataset.datos[:,-1]==1,0], dataset.datos[dataset.datos[:,-1]==1, 1], 'bo')

  plt.show()


def exe_REG_LOG(dataset=None,estrategia=None,n_epocas_to_exe=3,norm = True):
  
  table = "<table><tr> <th>N epocas</th> <th>Sin Normalizar</th>"
  if norm:
    table += "<th>Normalizando</th>"
  table += "</tr>"

  for i in n_epocas_to_exe:
    
    clasificador_sn=Clasificador.ClasificadorRegresionLogistica(False, i)
    errores_sn=clasificador_sn.validacion(estrategia, dataset, clasificador_sn)

    table += "<tr> <td>" + str(i)
    table += "</td><td>%.4f  ±%4.f</td>" % (np.mean(errores_sn), np.std(errores_sn))

    if norm:
      clasificador_sn=Clasificador.ClasificadorRegresionLogistica(True, i)
      errores_n=clasificador_sn.validacion(estrategia, dataset, clasificador_sn)

      table += "<td>%.4f  ±%4.f</td> </tr>" % (np.mean(errores_n), np.std(errores_n))
    else:
      table += "</tr>"

  table += "</table>"

  return table

def exe_REG_LOG_plot(dataset=None,estrategia=None,n_epocas_to_exe=3,data="none",norm=True):

  if norm:
    title = "REG LOG con " + data + " y n_epocas=" + str(n_epocas_to_exe) + "  "
  else:
    title = "REG LOG con " + data + " y n_epocas=" + str(n_epocas_to_exe) + "  "

  if norm:
    clasificador=Clasificador.ClasificadorRegresionLogistica(True, n_epocas_to_exe)
    title += "normalizado"
  else:
    clasificador=Clasificador.ClasificadorRegresionLogistica(False, n_epocas_to_exe)

  errores=clasificador.validacion(estrategia,dataset,clasificador)

  #índices de entrenamiento de la última partición
  ii = estrategia.particiones[-1].indicesTrain

  plotModel(dataset.datos[ii,0], dataset.datos[ii,1], dataset.datos[ii,-1] != 0, clasificador,title, dataset.diccionarios)
  plt.plot(dataset.datos[dataset.datos[:,-1]==0,0], dataset.datos[dataset.datos[:,-1]==0, 1], 'ro')
  plt.plot(dataset.datos[dataset.datos[:,-1]==1,0], dataset.datos[dataset.datos[:,-1]==1, 1], 'bo')

  plt.show()