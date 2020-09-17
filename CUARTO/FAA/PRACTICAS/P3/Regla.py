# -*- coding: utf-8 -*-
from abc import ABCMeta,abstractmethod
from Datos import Datos
from itertools import *
from lib_AG import *
import numpy as np
import collections
import math
import warnings
import random
import bisect
import copy


#Funcion para declarar una u otra dependiendo de un parámetro
def createRegla(esBinaria = True, list_tabla_intervalos=None, clases=None, fila=None):
	regla = None

	if esBinaria:
		regla = ReglaBinaria(list_tabla_intervalos,clases,fila)
	else:
		regla = ReglaEnteros(list_tabla_intervalos,clases,fila)

	return regla


#########################################################################################
#########################################################################################
class Regla(object):

	# Clase abstracta
	__metaclass__ = ABCMeta


	def getClase(self):
		return self.genes[-1]


	def getClaseInversa(self):

		if self.genes[-1] == self.clases[1]:
			return self.clases[0]
		else:
			return self.clases[1]


	@abstractmethod
	def seCumple(self,fila):
		pass


	@abstractmethod
	def cruce(self):
		pass


	@abstractmethod
	def mutacion(self,pmbit,n_intervalos):
		pass



#########################################################################################
#########################################################################################
class ReglaEnteros(Regla):

	def __init__(self, list_tabla_intervalos=None, clases=None, fila=None):
		self.genes = []
		self.clases = clases

		#Si fila != None significa que queremos una regla en concreto, sino, aleatoria
		if fila is not None:
			for i in range(len(fila)-1): #Recorremos cada columna excepto la clase

				#Añadimos un gen con el valor del intervalo donde pertenece fila[i]
				self.genes.append(get_i_interval(list_tabla_intervalos[i], fila[i]))

			#Por último, metemos la clase a la regla
			self.genes.append(fila[-1])

			#Transformamos a numpy
			self.genes = np.array(self.genes)


		#Creamos una regla aleatoria
		else:
			random.seed(None)
			for dic_intervalos in list_tabla_intervalos:

				# Cada gen será aleatorio (habrá mas probabilidad de valer 0)
				gen = np.random.choice([0,random.randint(1,get_k(dic_intervalos))],p=[0.90,0.10])
				self.genes.append(gen)

			#Por último, metemos la clase 1
			self.genes.append(random.choice(clases))
			#self.genes.append(clases[1])

			#Transformamos a numpy
			self.genes = np.array(self.genes)


	# Recibimos la fila en formato de regla
	def seCumple(self,fila):

		# Comparamos todo |len(genes)| menos la clase |-1|
		for i in range(len(self.genes)-1):
			# Si no se cumple una condicion -> false

			#Si es 0 se ignora
			if self.genes[i] != 0:
				# Esto es porque cada gen/condicion se juntan con AND ( cond1 AND cond2 AND ...)
				if self.genes[i] != fila.genes[i]:
					return False

		return True


	def mutacion(self,pmbit,n_intervalos):

		#Calculamos la probabilidad
		pmbit = float(pmbit/100)
		#Si por cualquier caso da > 1, la ponemos al 50%
		if pmbit > 1:
			pmbit = 0.5

		gen_de_clase = len(self.genes) - 1

		#Recorremos cada gen (excepto el de la clase) y solo lo mutamos con prob=pmbit
		for i in range(len(self.genes) - 1):
			if np.random.choice([True,False],p=[pmbit,1-pmbit]):
				self.genes[i] = np.random.choice([0,random.randint(1,n_intervalos[i])],p=[0.1,0.9])


#########################################################################################
#########################################################################################
class ReglaBinaria(Regla):

	def __init__(self, list_tabla_intervalos=None, clases=None, fila=None):
		self.genes =[]
		self.clases = clases

		#Si fila != None significa que queremos una regla en concreto, sino, aleatoria
		if fila is not None:
			for i in range(len(fila)-1): #Recorremos cada columna excepto la clase

				#Inicializamos a zero y ponemos a 1 el que corresponda
				gen = np.zeros(get_k(list_tabla_intervalos[i]))

				# Obtenemos el intervalo de fila[i]. El indice será intervalo - 1.
				# Ejemplo: [id0 id1 id2 id3 ... idx]
				# Si invervalo = 4 , idx = 3 .gen es [0 0 0 1 .. 0]
				idx = get_i_interval(list_tabla_intervalos[i], fila[i]) - 1

				gen[idx] = 1

				#Añadimos el gen
				self.genes.append(gen)

			#Por último, metemos la clase a la regla
			self.genes.append(fila[-1])

			#Transformamos a numpy
			self.genes = np.array(self.genes)


		#Creamos una regla aleatoria
		else:
			random.seed(None)
			for dic_intervalos in list_tabla_intervalos:

				gen = []
				for i in range(get_k(dic_intervalos)):
					gen.append(np.random.choice([0,1],p=[0.2,0.8]))
				gen = np.array(gen)

				# Cada gen será aleatorio (habrá mas probabilidad de valer 0)
				self.genes.append(gen)

			#Por último, metemos la clase 1
			self.genes.append(clases[1])

			#Transformamos a numpy
			self.genes = np.array(self.genes)



	# Recibimos la fila en formato de regla
	def seCumple(self,fila):

		# Comparamos todo |len(genes)| menos la clase |-1|
		for g in range(len(self.genes)-1):
			# Si no se cumple una condicion -> false

			#Si un gen es todo 0 la ignoramos
			suma = np.sum(self.genes[g])
			if suma != 0:
				#Recorremos cada gen [0 1 0 .. 0]
				for i in range(len(fila.genes[g])):
					#g = el gen , i = el bit del gen
					if fila.genes[g][i] == 1:
						if self.genes[g][i] != fila.genes[g][i]:
							return False

		return True


	def mutacion(self,pmbit,n_intervalos):

		#Calculamos la probabilidad
		pmbit = float(pmbit/100)
		#Si por cualquier caso da > 1, la ponemos al 50%
		if pmbit > 1:
			pmbit = 0.5

		gen_de_clase = len(self.genes) - 1

		#Recorremos cada gen (excepto la clase) y solo lo mutamos con prob=pmbit
		for g in range(len(self.genes) - 1):
			if np.random.choice([True,False],p=[pmbit,1-pmbit]):
				#g = el gen , i = el bit del gen
				for i in range(len(self.genes[g])):
					self.genes[g][i] = np.random.choice([0,1],p=[0.3,0.7])
