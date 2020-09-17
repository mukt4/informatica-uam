# -*- coding: utf-8 -*-
from abc import ABCMeta,abstractmethod
from Datos import Datos
from itertools import *
from Cromosoma import *
from Regla import *
from lib_AG import *
from EstrategiaParticionado import *
import numpy as np
import collections
import math
import warnings
import random
import bisect
import copy

class ClasificadorAlgoritmosGeneticos(object):
	
	def __init__(self,dataset=None, esBinaria = False, npoblacion=10,max_reglas=40,c_parada=97, max_g = 100,pc=85,pm=10,pmbit=5,pe=2,test=None):
		# Parámetros del clasificador
		self.poblacion = []
		self.npoblacion = npoblacion
		self.c_parada = c_parada
		self.max_g = max_g
		self.max_reglas = max_reglas
		self.esBinaria = esBinaria
		self.clases = np.unique(dataset[:,-1])
		self.test = test

		# Parámetros de las operaciones
		self.pc = pc
		self.pm = pm
		self.pmbit = pmbit
		self.pe = pe


		#Obtenemos los intervalos del dataset
		self.list_tabla_intervalos,self.n_intervalos = create_att_diccionary(dataset)
		# n_intervalos me da la k máxima para cada columna/atributo y clase de la regla.


	def inicializar_poblacion(self):

		self.poblacion = []
		#Creamos la poblacion aleatoria
		for _ in range(self.npoblacion):
			self.poblacion.append(Cromosoma(self.esBinaria,self.max_reglas,self.list_tabla_intervalos,self.clases))

		# Transformamos a numpy
		self.poblacion = np.array(self.poblacion)


	# Devuelve una lista de cromosomas con su peso (una matriz)
	def pob_with_fit(self,poblacion = None):
		#Si no recibe poblacion, usamos la de self
		if poblacion is None:
			poblacion = self.poblacion

		aux = []
		for individuo in poblacion:
			aux.append([individuo, individuo.fit])

		aux = np.array(aux)

		return aux


	#Ordena la poblacion recibida segun su fitness (descendente)
	def sort_a_poblacion(self, poblacion = None):
		aux = self.pob_with_fit(poblacion)

		#Ordenamos de mayor a menor segun el fitness
		aux = aux[aux[:,1].argsort(-1)][::-1]

		# Y nos olvidamos del fitness y guardamos la poblacion ordenada
		poblacion = aux[:,0]

		return poblacion


	# Ordena self.poblacion
	def sort_poblacion(self):
		self.poblacion = self.sort_a_poblacion(self.poblacion)
		return self.poblacion


	# Devuelve la población tras la seleccion de progenitores.
	def seleccion_progenitores(self):
		pob_aux = self.pob_with_fit()

		#Hacemos una copia, para no modificar la original (la poblacion self.poblacion)
		# De esta forma, los objetos son nuevos y no comparten direcciones de memoria en
		# cromosomas
		pob_aux = copy.deepcopy(pob_aux)
		total_fit = np.sum(pob_aux[:,1])


		#Calculamos la proporcion del fitness (acumulada)
		weights = []
		fitacumulado = 0
		for fit in pob_aux[:,1]:
			#Si el fitness total es 0 todos tienen la misma probabilidad (Todos son igual de malos)
			if total_fit == 0:
				fitacumulado += 1
				weights.append(fitacumulado/self.npoblacion)

			else:
				fitacumulado += fit
				weights.append(fitacumulado/total_fit)

		#Vamos eligiendo aleatoriamente proporcional al fitness
		poblacion_sp = []
		for i in range(self.npoblacion): #El tamanio es el mismo que la población original
			x=random.random()
			idx = bisect.bisect(weights,x) #Elige un indice aleatorio proporcional al fitness
			poblacion_sp.append(pob_aux[idx,0])

		#Transformamos a numpy
		poblacion_sp = np.array(poblacion_sp)

		return poblacion_sp


	# Devuelve la población tras la seleccion de progenitores.
	def cruce(self, poblacion):
		# Pc es la proporcion de elementos que harán cruce. (pc = 80 -> el 80%)
		n_cruce = int(self.npoblacion*(self.pc/100))

		#Si por cualquier caso n_cruce es mayor a npoblacion
		if n_cruce > self.npoblacion:
			n_cruce = self.npoblacion/2

		# En el cruce se hace por pares, por lo que n_cruce tiene que ser par
		if n_cruce % 2:
			#Si es 1, obligamos a que sea por lo menos 2
			if n_cruce == 1:
				n_cruce = 2
			else:
				n_cruce -= 1

		# Entones escogemos n_cruce individuos (sin repeticion) para cruzar
		aux_p = random.sample(list(poblacion),self.npoblacion) #Sirve para aleatorizarlos
		poblacion_c = aux_p[n_cruce:] # Elementos que no toca ser cruzados
		para_cruce = aux_p[:n_cruce]

		#Ahora vamos a crucer los que si tocan ser cruzados
		for prog_1,prog_2 in zip(para_cruce[::2],para_cruce[1::2]):

			vastago_1, vastago_2 = prog_1.cruce(prog_2)
			#Añadimos los vástagos a la poblacion cruzada
			poblacion_c.append(vastago_1)
			poblacion_c.append(vastago_2)

		#Transformar en numpy
		poblacion_c = np.array(poblacion_c)


		#Finalmente tenemos una poblacion donde se ha realizado cruce al pc de la población
		return poblacion_c


	def mutacion(self,poblacion):
		# Pm es la proporcion de elementos que harán mutacion. (pm = 5 -> el 5%)
		n_muta = int(self.npoblacion*(self.pm/100))

		#Si por cualquier caso n_muta es mayor a npoblacion
		if n_muta > self.npoblacion:
			n_muta = self.npoblacion/2


		# Entones escogemos n_muta individuos (sin repeticion) para mutar
		aux_p = random.sample(list(poblacion),self.npoblacion) #Sirve para aleatorizarlos
		poblacion_m = aux_p[n_muta:] # Elementos que no toca ser mutados

		for individuo in aux_p[:n_muta]:
			individuo.mutacion(self.pmbit,self.n_intervalos) #Mutamos
			poblacion_m.append(individuo) #Y lo metemos en la poblacion

		#Transformar en numpy
		poblacion_m = np.array(poblacion_m)

		#Finalmente tenemos una poblacion donde se ha realizado mutación al pm de la población
		return poblacion_m


	def entrenamiento(self,datostrain,diccionario):

		if self.test is not None:
			test_data  = open("test/" + self.test, "w")
			test_data.write("Generaciones\tf\tfmedio\n")

		self.inicializar_poblacion()
		siguiente_generacion = True
		i = 1
		while siguiente_generacion and i <= self.max_g:

			#Calculamos el fitness
			for cromosoma in self.poblacion:
				cromosoma.calculate_fit(datostrain)
			#Una vez hemos calculado el fitness Ordenamos
			self.sort_poblacion()

			#Despues, operamos y obtenemos una nueva poblacion
			poblacion = self.seleccion_progenitores()
			poblacion = self.cruce(poblacion)
			poblacion = self.mutacion(poblacion)

			#Calculamos el fitness de la nueva poblacion
			for cromosoma in poblacion:
				cromosoma.calculate_fit(datostrain)


			#Ordenamos la nueva poblacion
			poblacion = self.sort_a_poblacion(poblacion)


			#seleccion de supervivientes (Elitismo)
			elitismo = self.pe
			self.poblacion = np.append(poblacion[:-elitismo], self.poblacion[:elitismo])
			#self.poblacion = np.append(self.poblacion[:-elitismo], poblacion[:elitismo])

			#Ordenamos y vemos si se ha cumplido la condición de c_parada
			self.sort_poblacion()

			print("Generacion: ",i)
			print("Fitness mejor individuo: ",str(self.poblacion[0].fit))
			f_total = 0
			for cromosoma in self.poblacion:
				f_total += cromosoma.fit
			print("Fitness medio: ",str(f_total/self.npoblacion))

			if self.test is not None:
				test_data.write("%d\t%f\t%f\n" % (i,self.poblacion[0].fit,f_total/self.npoblacion))



			if self.poblacion[0].fit >= self.c_parada/100:
				siguiente_generacion = False

			i +=1

		if self.test is not None:
			test_data.close()


	def clasifica(self,datostest,diccionario):

		# Una vez hemos entrenado cogemos al mejor cromosoma.
		self.sort_poblacion() #Ordenamos otra vez por si acaso

		predicciones = self.poblacion[0].predicciones(datostest) #Predicciones del mejor individuo

		return predicciones


	# Obtiene el numero de aciertos y errores para devolver la tasa de fallo
	def error(self,datos,pred):
		#comparaa la lista de prediiciones con la clase de test

		# Aqui se compara la prediccion (pred) con las clases reales y se calcula el error
		#si es igual sumas uno y si no nose suma

		ntest = len(pred)
		nerrors = 0

		for i in range(ntest):
			if pred[i] != datos[i,-1]:
				nerrors += 1

		percent_errors = float(nerrors)/float(ntest)

		return percent_errors


	# Realiza una clasificacion utilizando una estrategia de particionado determinada
	def validacion(self,particionado,dataset,seed=None):

		# Creamos las particiones siguiendo la estrategia llamando a particionado.creaParticiones
		# - Para validacion cruzada: en el bucle hasta nv entrenamos el clasificador con la particion de train i
		# y obtenemos el error en la particion de test i
		# - Para validacion simple (hold-out): entrenamos el clasificador con la particion de train
		# y obtenemos el error en la particion test
		particionado.creaParticiones(dataset, seed)
		list_errors = []
		for particion in particionado.particiones:

			train = dataset.extraeDatos(particion.indicesTrain)
			test = dataset.extraeDatos(particion.indicesTest)

			self.entrenamiento(train, dataset.diccionarios)
			predicciones= self.clasifica(test, dataset.diccionarios)

			list_errors.append(self.error(test,predicciones))

		return list_errors


def main(dataset_name, poblacion, max_generaciones, test):
	dataset=Datos('data/' + dataset_name)
	clf = ClasificadorAlgoritmosGeneticos(dataset.datos,False,npoblacion=poblacion,max_g=max_generaciones, test=test)

	print("Empezamos...")
	# Una sola partición de momento
	estrategia=ValidacionSimple(1)

	#Validamos
	tasa_errores = clf.validacion(estrategia,dataset)

	print("Tasa de error: ",tasa_errores)
	print("Tasa de acierto: ", 1 - tasa_errores[0])
