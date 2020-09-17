# Practica realizada por Tomas Higuera Viso y Alejandro Naranjo Jimenez

import numpy as np

class Datos:
  
	TiposDeAtributos=('Continuo','Nominal')
 
  # TODO: procesar el fichero para asignar correctamente las variables tipoAtributos, nombreAtributos, nominalAtributos, datos y diccionarios
  # NOTA: No confundir TiposDeAtributos con tipoAtributos
	def __init__(self, nombreFichero):
	    # Abrimos el fichero en modo lectura 
		file = open(nombreFichero, "r")

	  	# Realizamos una comprobacion para ver si el fichero se ha abierto en modo lectura
		if file.mode == "r":
	  		# Leemos la primera linea del fichero y nos guardamos el numero de instancias
			instances = int(file.readline())

	  		# A continuacion leemos los atributos
			atributos = file.readline()

	  		# Leemos el tipo de los atributos
			types = file.readline()

	  		# Una vez leidos los atributos los guardamos en la clase datos, aunque antes comprobamos que sean o continuo o nominal
			types_list = types.strip().split(",")

	  		# Una vez tenemos la lista de los tipos los guardamos comprobando antes si se cumple la condicion del tipo
	  		# Generamos la lista de tipo de atributos
			self.tipoAtributos = []

	  		# Generamos la lista nominal de atributos
			self.nominalAtributos = []

			for _a in types_list:
	  			# Comprobamos si el tipo de atributo es continuo para aniadir el booeleano correcto en nominal
				if _a == self.TiposDeAtributos[0]:
					self.tipoAtributos.append(_a)
					self.nominalAtributos.append(False)
	  			# Comprobamos si el tipo de atributo es nominal
				elif _a == self.TiposDeAtributos[1]:
					self.tipoAtributos.append(_a)
					self.nominalAtributos.append(True)

	  			# Si el atributo no es ni nominal ni continuo lanzamos excepcion
				else:
					raise ValueError("Valor de atributo no soportado : " + a)

	  		# Si no se ha lanzado ninguna excepcion anadimos el nombre de los atributos
			self.nombreAtributos = atributos.strip().split(",")

	  		# A continuacion anadimos los datos en la clase
	  		# Creamos una lista de datos sucia que luego parsearemos a numpy
			_datos_sucios_lista = []
			for linea in file:
				_datos_sucios_lista.append(linea.strip().split(","))
		   
		    # Parsing de los datos a un array de numpy
			_datos_sucios = np.array(_datos_sucios_lista)

		    # Pasamos a rellenar la lista de diccionarios de la clase
		    # Incializamos el atributo de la clase
			self.diccionarios = []
			self

		    # Realizamos un bucle que recorra todos los atributos
			for i in range(len(self.nombreAtributos)):
		    	# Miramos si la columna es de atributos nominales
				if self.nominalAtributos[i]:
		        	# cogemos la columna i, quitamos los repetidos y la ordenamos)
					_aux = sorted(np.unique(_datos_sucios[:, i]))
		        	# Pasamos la lista a diccionario clave-valor
					_dictaux = {_aux[i] :i for i in range(0, len(_aux))}
		      	# Si el atributo 
				else:
					_dictaux = {}

				self.diccionarios.append(_dictaux)

		    # Creamos la matriz de datos y recorremos cada fila traduciendo los nominales
		    # Creamos una matriz auxiliar que luego parsearemos a numpy
			_matrizaux = []
			for i in range(instances):
		      	# Limpiamos aux
				_aux.clear()
				for j in range(len(self.nombreAtributos)):
					_dato = (_datos_sucios[i])[j]
		        	# Miramos si la columna es de atributos nominales
					if self.nominalAtributos[j]:
		          		# Buscamos la clave en el diccionario
						for key, value in self.diccionarios[j].items():
							if _dato == key:
								_aux.append(float(value))
		        	# Si no, solo insertamos el elemento
					else:
						_aux.append(float(_dato))
		          
		      	# Hacemos una copia de la lista
				_matrizaux.insert(i, _aux.copy())

		    # Parsing de la matriz a numpy
			self.datos = np.array(_matrizaux)
		    #Cerramos el  fichero
			file.close()
		
		# Si el fichero no se ha abrido de manera correcta lanzamos excepcion
		else:
			raise TypeError("El fichero no se ha abierto de manera correcta : " + f.mode)
			file.close()


	# TODO: implementar en la práctica 1
	def extraeDatos(self, idx):
		return self.datos[idx]


  
