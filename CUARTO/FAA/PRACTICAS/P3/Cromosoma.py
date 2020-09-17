# -*- coding: utf-8 -*-
from abc import ABCMeta,abstractmethod
from Datos import Datos
from itertools import *
from Regla import *
from lib_AG import *
import numpy as np
import collections
import math
import warnings 
import random
import bisect
import copy

class Cromosoma(object):

	def __init__(self, esBinaria = True, max_reglas=10, list_tabla_intervalos=None, clases=None):
		self.reglas = []
		self.fit = 0 # NºAciertos/Total
		self.list_tabla_intervalos = list_tabla_intervalos
		self.esBinaria = esBinaria

		random.seed(None)
		for _ in range(random.randint(1, max_reglas)):
			self.reglas.append(createRegla(self.esBinaria,self.list_tabla_intervalos, clases))

		# Transformamos a numpy
		self.reglas = np.array(self.reglas)


	def calculate_fit(self,datos):

		pred = self.predicciones(datos)
		total = len(datos)

		# Si no se consigue realizar ninguna prediccion dejamos el fit a 0
		if not pred:
			self.fit = 0
			return self.fit

		naciertos = 0
		for i in range(total):
			if pred[i] == datos[i,-1]:
				naciertos += 1

		self.fit = float(naciertos)/float(total)
		return self.fit


	# Recibe uns filas de datos y devuelve lo que predice para cada una
	def predicciones(self,datos):

		predicciones = [] #Contendrá una prediccion del cromosoma por cada fila de Datos


		#Primero creamos cada fila en una regla y la comparamos con las reglas del cromosomas
		for fila in datos:
			regla_fila = createRegla(self.esBinaria,self.list_tabla_intervalos,None,fila)
			# Ahora tenemos que ver que predice el cromosoma con la fila (para cada una de las reglas del cromosoma)
			# Y escoger la clase mayoritaria que predicen cada una de las reglas.
			predic_reglas = []
			for regla in self.reglas:
				if regla.seCumple(regla_fila):
					predic_reglas.append(regla.getClase()) #Si se cumple predice la clase

			#Si todas las reglas dan error devolvemos un None (añadiendo un predicion que significa nada)
			if not predic_reglas:
				#Metemos un valor indicando que no ha predicho nada
				predicciones.append(None)

			#Si alguna se cumple, pues la mayoritaria
			else:
				# La clase mayoriataria es lo que predice en esa fila
				counts = np.bincount(predic_reglas)
				predicciones.append(np.argmax(counts))

		return predicciones


	# Cruce en 1 punto
	def cruce(self,progenitor_2):
		
		#El punto de cruce se hace en un punto aleatorio
		min_reglas = min(len(self.reglas), len(progenitor_2.reglas))
		if min_reglas == 1:
			min_reglas += 1
		punto_c = random.randint(1,(min_reglas-1))

		# Reglas de 1 que pasan a 2
		reglas1_to_2 = self.reglas[punto_c:]

		# Cruzamos los 2 cromosomas. Cruce en n puntos (seguidos)
		self.reglas = np.append(self.reglas[:punto_c],progenitor_2.reglas[punto_c:])
		progenitor_2.reglas = np.append(progenitor_2.reglas[:punto_c],reglas1_to_2)

		#Devolvemos los vástagos
		return self,progenitor_2


	#Mutamos una regla aleatoria y la regla elegida con probablidiad de cada gen pmbit
	def mutacion(self,pmbit, n_intervalos):

		#Mutamos una regla aleatoria
		idx = random.randint(0,len(self.reglas)-1)
		self.reglas[idx].mutacion(pmbit,n_intervalos)
