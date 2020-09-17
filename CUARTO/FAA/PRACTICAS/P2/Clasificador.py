# -*- coding: utf-8 -*-

# Practica realizada por Tomas Higuera Viso y Alejandro Naranjo Jimenez

from abc import ABCMeta,abstractmethod
import numpy as np
import collections
import math
import numpy as np
from sklearn.preprocessing import normalize
from scipy.spatial import distance
from random import uniform

class Clasificador:
  
	# Clase abstracta
	__metaclass__ = ABCMeta
  
	# Metodos abstractos que se implementan en casa clasificador concreto
	@abstractmethod
	# TODO: esta funcion debe ser implementada en cada clasificador concreto
	# datosTrain: matriz numpy con los datos de entrenamiento
	# atributosDiscretos: array bool con la indicatriz de los atributos nominales
	# diccionario: array de diccionarios de la estructura Datos utilizados para la codificacion de variables discretas
	def entrenamiento(self,datosTrain,atributosDiscretos,diccionario):
		pass


	@abstractmethod
	# TODO: esta funcion debe ser implementada en cada clasificador concreto
	# devuelve un numpy array con las predicciones
	def clasifica(self,datosTest,atributosDiscretos,diccionario):
		pass


	# Obtiene el numero de aciertos y errores para calcular la tasa de fallo
	# TODO: implementar
	def error(self,datos,pred):
		# Aqui se compara la prediccion (pred) con las clases reales y se calcula el error    

		nac = len(pred)
		numerr = 0 
		#En caso de ser igual se suma uno
		for i in range(nac):
			if pred[i] != datos[i,-1]:
				numerr += 1

		porcen = float(numerr)/float(nac)
		return porcen



  
	# Realiza una clasificacion utilizando una estrategia de particionado determinada
	# TODO: implementar esta funcion
	def validacion(self,particionado,dataset,clasificador,seed=None):
		# Creamos las particiones siguiendo la estrategia llamando a particionado.creaParticiones
		# - Para validacion cruzada: en el bucle hasta nv entrenamos el clasificador con la particion de train i
		# y obtenemos el error en la particion de test i
		# - Para validacion simple (hold-out): entrenamos el clasificador con la particion de train
		# y obtenemos el error en la particion test. Otra opción es repetir la validación simple un número especificado de veces, obteniendo en cada una un error. Finalmente se calcularía la media.
		
		errlist = []
		particionado.creaParticiones(dataset, seed)


		for particion in particionado.particiones:
			train = dataset.extraeDatos(particion.indicesTrain)
			test = dataset.extraeDatos(particion.indicesTest)
			clasificador.entrenamiento(train, dataset.nominalAtributos,dataset.diccionarios)
			pred= clasificador.clasifica(test, dataset.nominalAtributos,dataset.diccionarios)
			errlist.append(clasificador.error(test,pred))

		return errlist		 

##############################################################################

class ClasificadorNaiveBayes(Clasificador):

	def __init__(self, doLaplace=False):
		self.doLaplace = doLaplace
		self.trainTable = []
		self.prioriTable = {}
		
	def entrenamiento(self,datostrain,atributosDiscretos,diccionario):
		
		#Conseguimos los diccionorarios
		tam = len(diccionario)
		#Conseguimos el ultimo atribito que será la clase
		clase = len(diccionario[-1])
		#Num de filas
		nfilas = datostrain.shape[0]
		#Comprobamos que el numero de filas es mayor que cero
		if nfilas<=0:
			raise ValueError('Error. El numero de filas no puede ser cero.')
		
		#Miramos todas las claves de la clase, comprobamnos el valor k-esimo de la clave y se calculan los priori(contando repeticiones y dividiendo entre las filas)
		for key in diccionario[-1].keys():
			clave=diccionario[-1][key]
			self.prioriTable[key]=((datostrain[:,-1] == clave).sum())/nfilas

		#Miramos si es continio o nominal
		for i in range(tam-1):
			if atributosDiscretos[i]:
				numValAt = len(diccionario[i])
				tabla = np.zeros((numValAt, clase))
				#el atributo es el indice i,el valor de la clase es la columna
				for fila in datostrain:
					fil=int(fila[i])
					col=int(fila[-1])
					#num veces que se repite el valor del atributo con esa clase
					tabla[fil, col] +=1
				#Si se activa el flag laplace se suma 1 a las celdas
				if self.doLaplace and np.any(tabla==0):
					tabla+=1
				#Calculamos las probabilidades
				cont=tabla.sum(axis=0)
				for i in range(tabla.shape[0]):
					for j in range(tabla.shape[1]):
						tabla[i][j]/=cont[j]
			#Si son continuos cogemos todas las clave y hacemos la media y la desiviacion tipica
			else:
				tabla = np.zeros((2, clase))
				for key in diccionario[-1].keys():
					val=int(diccionario[-1][key])
					#Media
					tabla[0, val] = np.mean(datostrain[:,i])
					#Desviación típica
					tabla[1, val] = np.std(datostrain[:,i])

			self.trainTable.append(tabla)

	def clasifica(self,datostest,atributosDiscretos,diccionario):
		
		listpredic = []
		#Recorremos todos los datos
		for test in datostest:
			posteriori={}
			#Recorremos todas las clases para calcular la probabilidad MV
			for key in diccionario[-1].keys():
				prob=1
				val=diccionario[-1][key]
				for i in range(len(test)-1):
					#Conseguimos el valor del atributo y la clase y lo dividimos por el sumatorio de la columna
					if atributosDiscretos[i]:
						verosimilitud=self.trainTable[i][int(test[i]), val]
						evidencia=sum(self.trainTable[i][:,val])
						prob*=(verosimilitud/evidencia)
					# en atributos continuos aplicamos la formula (1/((2*pi*desviacion)^1/2))*e{(-(valor-media)^2)/2*desviacion)
					else:
						sqrt=math.sqrt(2*math.pi*self.trainTable[i][1,val])
						exp=math.exp(-(pow((test[i]-self.trainTable[i][0,val]),2))/(2*self.trainTable[i][1,val]))
						prob*=(exp/sqrt)
				
				#Multiplicamos por la prob a priori
				prob*=self.prioriTable[key]
				# guardamos para poder comparar con el resto 
				posteriori[key]=prob
			#Guardamos el valor de la clase con mayor probabilidad
			mayorprob=max(posteriori, key=posteriori.get)
			listpredic.append(diccionario[-1][mayorprob])
		return np.array(listpredic)			

#Funcion que normaliza los datos
def normalizarDatos(clasificador,datos): 
	# Guardamos el tamanio de nuestra matriz de datos
	tamanio = datos.shape[1] - 1
	matriz_normalizada = datos

	for i in range(tamanio):
		# Si el atributo no es un diccionario vacio significara que estamos ante
		# un atributo continuo
		if clasificador.desvMedList[i]:
			# Guardamos los valores e la desviacion y la media
			mean = clasificador.desvMedList[i]['mean']
			std = clasificador.desvMedList[i]['std']
			matriz_normalizada[ : , i] = (datos[ : , i] - float(mean)) / float(std)

		# Si el diccionario esta vacio se tratara de un atrbuto discreto por lo que
		# no tiene sentido normalizar
		else:
			# Imprimimos mensaje de error
			print("Estas intentando normalizar con atributos discretos.")
			# Si es un atributo discreto
			matriz_normalizada[ : , i] = datos[ : , i] #Si es discreto no tiene sentido normalizar

	return matriz_normalizada

# Esta funcion calculara las medias y desviaciones tipicas
# de cada atributo continuo
def calcularMediasDesv(clasificador, datostrain):
	# Creamos un diccionario vacio que contendra la media y la desviacion tipica
	media_desviacion = {}
	# Calculamos la media utilizando las funciones de numpy
	media_desviacion["mean"] = np.mean(datostrain)	
	# Calculamos la desviacion tipica utilizando las funciones de numpy
	media_desviacion["std"] = np.std(datostrain)

	# Guardamos la desviacion tipica y la media en el clasificador
	clasificador.desvMedList.append(media_desviacion)

# Algoritmo vecinos proximos
class ClasificadorVecinosProximos(Clasificador):

	def __init__(self, k=2, Normal=False):
		self.k = k
		self.Normal = Normal
		self.trainTable = None 
		self.desvMedList = [] 
 
	def entrenamiento(self,datostrain,atributosDiscretos,diccionario):

		del self.desvMedList[:]
		del self.trainTable

		if self.Normal:
			size=len(diccionario) - 1 
			for i in range(size):
				if atributosDiscretos[i]:
					self.desvMedList.append({})
				else:
					calcularMediasDesv(self,datostrain[:,i])

			self.trainTable = normalizarDatos(self,datostrain)

		else:
			self.trainTable = datostrain

	def clasifica(self,datostest,atributosDiscretos,diccionario):

		if self.Normal:
			testList = normalizarDatos(self,datostest) 

		else:
			testList = datostest

		clasesList = []

		for fil in testList:
			ListDistances = []
			for i in range(self.trainTable.shape[0]):
				ListDistances.append(distance.euclidean(self.trainTable[i,:-1], [fil[:-1]]))
			sortedDistancesIndex = np.argsort(ListDistances)
			nearClass = self.trainTable[sortedDistancesIndex[0:self.k], -1]
			clasesWithCount = np.bincount(nearClass.astype(int)) 
			clasesList.append(int(clasesWithCount.argmax()))

		total = np.array(clasesList)

		return total


# Algoritmo regresion logistica
class ClasificadorRegresionLogistica(Clasificador):

	def __init__(self, Normal = False, epoch=4, num_apren = 1):
		self.wfin = np.array(())
		self.num_apren = num_apren
		self.epoch = epoch
		self.Normal = Normal
		self.desvMedList = []
		self.trainTable = None

	def reg_log(self, datos, w, i):

		x = np.array(())
		x = np.append(x ,np.append(1, datos[i][:-1]))
		wx = np.dot(np.transpose(w), x)
		
		nxt = 1/(1 + np.exp(-wx))

		return x,nxt


	def entrenamiento(self,datostrain,atributosDiscretos,diccionario):
		del self.desvMedList[:]
		del self.trainTable 

		ejem, col = datostrain.shape
		w = np.array(())

		if self.Normal:
			size=len(diccionario) - 1 

			for i in range(size):
				if atributosDiscretos[i]:
					self.desvMedList.append({})
				else:
					calcularMediasDesv(self,datostrain[:,i])
			self.trainTable = normalizarDatos(self,datostrain)
		else:
			self.trainTable = datostrain

		for i in range(col):
 			w = np.append(w, uniform(-0.5,0.5))

		for ie in range(self.epoch):
			for i in range(ejem):
				x,nxt = self.reg_log(datostrain, w, i)
				for d in range(col):
					w[d] = w[d] - self.num_apren*(nxt - datostrain[i, -1])* x[d]

		self.wfin = w
    
	def clasifica(self,datostest,atributosDiscretos,diccionario):
		ejem, col = datostest.shape
		predict = []
    
		C1 = 1
		C2 = 0


		if self.Normal:
			auxtest = normalizarDatos(self,datostest) 
		else:
			auxtest = datostest

		for i in range(ejem):
			x,nxt = self.reg_log(auxtest, self.wfin, i) 

			if nxt <= 0.5:
				predict.append(C2)
			elif nxt > 0.5:
				predict.append(C1)
		
		return np.array(predict)

