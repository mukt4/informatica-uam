# Practica realizada por Tomas Higuera Viso y Alejandro Naranjo Jimenez

from abc import ABCMeta,abstractmethod
import numpy as np
import math
import random
import itertools

class Particion():

	# Esta clase mantiene la lista de índices de Train y Test para cada partición del conjunto de particiones  
	def __init__(self):
		self.indicesTrain=[]
		self.indicesTest=[]

	#####################################################################################################

class EstrategiaParticionado:
	# Clase abstracta
	__metaclass__ = ABCMeta

	# Atributos: deben rellenarse adecuadamente para cada estrategia concreta: nombreEstrategia, numeroParticiones, listaParticiones. Se pasan en el constructor 

	@abstractmethod
	# TODO: esta funcion deben ser implementadas en cada estrategia concreta  
	def creaParticiones(self,datos,seed=None):
		pass


#####################################################################################################

class ValidacionSimple(EstrategiaParticionado):
	# Constructor de la clase validacion simple
	def __init__(self, nParticiones = 1, porcentaje = 25):
		self.nombreEstrategia = "Validacion simple"
		self.numeroParticiones = nParticiones
		# Este procenraje sera el utilizado en la parte de train
		self.porcentaje = porcentaje
		# Creamos una lista que contendra las particiones de los datos
		self.particiones = []
		for i in range(nParticiones):
			self.particiones.append(Particion())

	# Crea particiones segun el metodo tradicional de division de los datos segun el porcentaje deseado.
	# Devuelve una lista de particiones (clase Particion)
	# TODO: implementar
	def creaParticiones(self,datos,seed=None):
		# Generamos una semilla para crear numeros aleatorios
		random.seed(seed)
		# Creamos una lista de indices que sera la que utilizaremos para obtener los valores de las particiones
		indices_list = np.arange(len(datos.datos))
		# Calculamos el numero de datos que introduciremos en la parte de train, obtenemos el suelo de este resultado para obtener valores redondo
		datos_train = math.floor(len(datos.datos) * self.porcentaje / 100)
		# Una vez calculados los datos que inteoduciremos en train pasamos a realizar el bucle que rellenara las particiones
		for i in range(self.numeroParticiones):
			# Antes de empezar a generar las particiones realizamos un shuffle entre los indices para que estos
			# no esten en orden
			indices_list = np.random.permutation(indices_list)
			# Una vez hecho el shuffle a la lista aniadimos los datos utilizando indexacion
			self.particiones[i].indicesTrain = indices_list[: datos_train]
			self.particiones[i].indicesTest = indices_list[datos_train : ]
		# Por ultimo devolvemos la lista de particiones creadas
		return self.particiones
  
  
#####################################################################################################      
class ValidacionCruzada(EstrategiaParticionado):
	# Constructor de la clase validacion cruzada
	def __init__(self, nParticiones):
		self.nombreEstrategia = "Validacion cruzada"
		self.numeroParticiones = nParticiones
		# Creamos una lista que contendra las particiones de los datos
		self.particiones = []
		for i in range(nParticiones):
			self.particiones.append(Particion())

	# Crea particiones segun el metodo de validacion cruzada.
	# El conjunto de entrenamiento se crea con las nfolds-1 particiones y el de test con la particion restante
	# Esta funcion devuelve una lista de particiones (clase Particion)
	# TODO: implementar
	def creaParticiones(self,datos,seed=None): 
		# Generamos una semilla para crear numeros aleatorios  
		random.seed(seed)
		# Al igual que en validacion simple creamos una lista de indices que sera la que utilicemos para 
		# obtener los valores de los indices
		indices_list = np.arange(len(datos.datos))
		# Realizamos un shuffle con los diferente indices posibles
		indices_list = np.random.permutation(indices_list)
		# Calculamos si el numero de particiones divide los datos de manera exacta
		if len(indices_list) % self.numeroParticiones != 0:
			# Si la division no es exacta pasamos a aniadir indices a la lista
			indices_extra = random.sample(range(0, len(datos.datos)), 10 - len(indices_list) % self.numeroParticiones)
			indices_list = np.append(indices_list, indices_extra)
		# Generamos nParticiones sublistas de la lista de indices
		indices_list = np.split(indices_list, self.numeroParticiones)
		# Una vez generadas las sublistas pasamos a aniadirlas a las particiones
		for i in range(self.numeroParticiones):
			# Aniadimos la particion de test y a continuacion aniadimos las de prueba
			self.particiones[i].indicesTest.extend(indices_list[i])
			# Aniadimos ahora las particiones de train, primero la primera parte
			self.particiones[i].indicesTrain.extend(list(itertools.chain.from_iterable(indices_list[:i])))
			# A continuacion aniadimos la segunda parte a los indices de entrenamiento
			self.particiones[i].indicesTrain.extend(list(itertools.chain.from_iterable(indices_list[i+1:])))
		# Devolvemos la lista de particiones creadas
		return self.particiones

