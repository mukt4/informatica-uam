# Practica realizada por Tomas Higuera Viso y Alejandro Naranjo Jimenez

from abc import ABCMeta,abstractmethod
import numpy as np
import collections
import math

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
