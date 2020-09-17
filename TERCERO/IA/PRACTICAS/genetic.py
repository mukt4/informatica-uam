import random
import numpy as np
from copy import copy

MAX_HEURISTICAS = 14
MAX_JUGADORES = 100
NUM_BITS = 7
NUM_POBLACIONES = 10
MAX_FILAS = 6
MAX_COLUMNAS = 7
JUGADOR_PRIMERO = 'X'
JUGADOR_SEGUNDO = 'O'

# El valor de la heuristica total se encuentra acotado entre -42 y 42

class Jugador():
	def __init__(self, numero):
		self.chromosome = Chromosome()
		self.nombre = "Jugador " + str(numero)
		self.victorias = 0

	def set_chromosome(self, chromosome):
		self.chromosome = chromosome

	def get_chromosome(self):
		return self.chromosome

	def set_victorias(self, numero):
		self.victorias = numero

	def get_victorias(self):
		return self.victorias

	def get_nombre(self):
		return self.nombre

class Poblacion():
	def __init__(self):
		self.jugadores = []

	def aniadir(self, jugador):
		self.jugadores.append(jugador)

	def primera_generacion(self):
		for i in range(MAX_JUGADORES):
			self.jugadores.append(Jugador(i))

	def get_jugadores(self):
		return self.jugadores

	def get_jugador(self, indice):
		return self.jugadores[indice]

class Chromosome():
	def __init__(self):
		self.cadena = []
		valores = np.random.multinomial(100, np.ones(MAX_HEURISTICAS)/MAX_HEURISTICAS, size=1)[0]
		for valor in valores:
			self.cadena.append(Gen(valor))

	def set_gen(self, position, gen):
		self.cadena[position] = gen

	def get_gen(self, position):
		return self.cadena[position]

class Gen():
	def __init__(self,numero):
		self.bits = (NUM_BITS*"0" + bin(numero)[2:])[-NUM_BITS:]

	def get_numero(self):
		total = 0
		for i in range(len(self.bits)):
			total += 2**(6-i)
		return total

def ranking(poblacion):
	return poblacion.jugadores.sort(key=lambda x: x.victorias)

def generar_hijos(jugador1, jugador2):	
	hijo = Jugador()
	nuevo_cromosoma = Chromosome()

	for i in range(MAX_HEURISTICAS):
		nuevo_gen = mezclar_gen(jugador1.get_chromosome.get_gen(i), jugador2.get_chromosome.get_gen(i))
		nuevo_cromosoma.set_gen(i, nuevo_gen)

	hijo.set_chromosome(nuevo_cromosoma)

	return hijo

def mezclar_gen(gen1, gen2):
	nuevo_gen = []
	nuevo_gen.append(gen1[0])
	nuevo_gen.append(gen1[1])
	# Creamos mutacion haciendo XOR
	for i in range(3):
		if(gen1[2+i] == gen2[2+i]):
			nuevo_gen.append(0)
		else:
			nuevo_gen.append(1)
	nuevo_gen.append(gen2[5])
	nuevo_gen.append(gen2[6])
	return nuevo_gen		

# Todas las heuristicas del problema
# Heuristica que comprueba las fichas en diagonal consecutivas del jugador contrario
# Entre 0 y -3
# Hay que corregir esta vaina igual que la heuristica 4
def heuristica1(tablero, jugador):
	minimo = 0

	if jugador == JUGADOR_PRIMERO:
		comprobar = JUGADOR_SEGUNDO
	else:
		comprobar = JUGADOR_PRIMERO

	for i in range(MAX_FILAS):
		cuenta = 0

		for j in range(MAX_COLUMNAS):
			if tablero[j][i] == comprobar:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0

	return -minimo

# Heuristica que comprueba las fichas en horizontal consecutivas del jugador contrario
# Entre 0 y -3
def heuristica2(tablero, jugador):
	minimo = 0

	if jugador == JUGADOR_PRIMERO:
		comprobar = JUGADOR_SEGUNDO
	else:
		comprobar = JUGADOR_PRIMERO

	for i in range(MAX_FILAS):
		cuenta = 0

		for j in range(MAX_COLUMNAS):
			if tablero[j][i] == comprobar:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0

	return -minimo

	

# Heuristica que comprueba las fichas en vertical consecutivas del jugador contrario
# Entre 0 y -3
def heuristica3(tablero, jugador):
	minimo = 0

	if jugador == JUGADOR_PRIMERO:
		comprobar = JUGADOR_SEGUNDO
	else:
		comprobar = JUGADOR_PRIMERO

	for i in range(MAX_COLUMNAS):
		cuenta = 0

		for j in range(MAX_FILAS):
			if tablero[i][j] == comprobar:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0

	return -minimo


# Heuristica que comprueba las fichas en diagonal consecutivas del jugador actual
# Entre 0 y 3
# Hay que corregir esta vaina
def heuristica4(tablero, jugador):
	minimo = 0

	for i in range(MAX_FILAS):
		cuenta = 0

		for j in range(MAX_COLUMNAS):
			if tablero[j][i] == jugador:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0

	return minimo

# Heuristica que comprueba las fichas en horizontal consecutivas del jugador actual
# Entre 0 y 3
def heuristica5(tablero, jugador):
	minimo = 0

	for i in range(MAX_FILAS):
		cuenta = 0

		for j in range(MAX_COLUMNAS):
			if tablero[j][i] == jugador:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0

	return minimo

# Heuristica que comprueba las fichas en vertical consecutivas del jugador actual
# Entre 0 y 3
def heuristica6(tablero, jugador):
	minimo = 0

	for i in range(MAX_COLUMNAS):
		cuenta = 0

		for j in range(MAX_FILAS):
			if tablero[i][j] == jugador:
				cuenta+= 1
			else:
				if cuenta > minimo:
					minimo = cuenta
				cuenta = 0
				
	return minimo

# Heuristica que devuelve las fichas de la columa 1
# Entre -3 y 3
def heuristica7(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[0][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 2
# Entre -3 y 3
def heuristica8(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[1][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 3
# Entre -3 y 3
def heuristica9(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[2][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 4
# Entre -3 y 3
def heuristica10(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[3][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 5
# Entre -3 y 3
def heuristica11(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[4][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 6
# Entre -3 y 3
def heuristica12(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[5][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Heuristica que devuelve las fichas de la columa 7
# Entre -3 y 3
def heuristica13(tablero, jugador):
	cuenta = -3
	for i in range(MAX_FILAS):
		if(tablero[6][i] != ' '):
			cuenta+= 1
		else:
			return cuenta

# Turnos pares X y turnos impares O

class Tablero():
	def __init__(self):
		self.tablero = []
		self.jugador = 'X'
		for i in range(MAX_COLUMNAS):
			self.tablero.append([' '] * MAX_FILAS)

	def get_tablero(self):
		return self.tablero

	def movimiento(self, columna):
		for i in range(MAX_FILAS):
			if self.tablero[columna][i] == ' ':
				self.tablero[columna][i] = self.jugador
				if self.jugador == JUGADOR_PRIMERO:
					self.jugador = JUGADOR_SEGUNDO
				else:
					self.jugador = JUGADOR_PRIMERO
				return True
		return False

	def movimientos_posibles(self):
		movimientos = []
		for i in range(MAX_COLUMNAS):
			if self.comprobar_movimiento(i) == True:
				movimientos.append(i)
		return movimientos


	def comprobar_movimiento(self, columna):
		for i in range(MAX_FILAS):
			if(self.tablero[columna][i] == ' '):
				return True

		return False

	def comprobarVictoria(self):
		if self.jugador == JUGADOR_PRIMERO:	
			jugador_anterior = JUGADOR_SEGUNDO
		else:
			jugador_anterior = JUGADOR_PRIMERO
	
	    # check horizontal spaces
		for y in range(MAX_FILAS):
			for x in range(MAX_COLUMNAS - 3):
				if self.tablero[x][y] == jugador_anterior and self.tablero[x+1][y] == jugador_anterior and self.tablero[x+2][y] == jugador_anterior and self.tablero[x+3][y] == jugador_anterior:
					return True

	    # check vertical spaces
		for x in range(MAX_COLUMNAS):
			for y in range(MAX_FILAS - 3):
				if self.tablero[x][y] == jugador_anterior and self.tablero[x][y+1] == jugador_anterior and self.tablero[x][y+2] == jugador_anterior and self.tablero[x][y+3] == jugador_anterior:
					return True

	    # check / diagonal spaces
		for x in range(MAX_COLUMNAS - 3):
			for y in range(3, MAX_FILAS):
				if self.tablero[x][y] == jugador_anterior and self.tablero[x+1][y-1] == jugador_anterior and self.tablero[x+2][y-2] == jugador_anterior and self.tablero[x+3][y-3] == jugador_anterior:
					return True

	    # check \ diagonal spaces
		for x in range(MAX_COLUMNAS - 3):
			for y in range(MAX_FILAS - 3):
				if self.tablero[x][y] == jugador_anterior and self.tablero[x+1][y+1] == jugador_anterior and self.tablero[x+2][y+2] == jugador_anterior and self.tablero[x+3][y+3] == jugador_anterior:
					return True

		return False

	def simular_movimiento(self, columna):
		copia = copy(self)
		for i in range(MAX_FILAS):
			if(copia.tablero[columna][i] == ' '):
				return copia

		return False

	def tablero_completo(self):
		if len(self.movimientos_posibles()) == 0:
			return True
		else:
			return False

	def generarMovimientoSiguiente(self, columna):
		tableros = []
		copia = self.simular_movimiento(columna)

		for i in self.movimientos_posibles():
			tableros.append(self.simular_movimiento(i))

		return tableros

	def realizar_movimiento(self, jugador):
		max_puntuacion = 0
		mejor_movimiento = 0

   		# Obtenemos los posibles movimientos del jugador
		posibles = self.movimientos_posibles()

   		# Obtenemos los diferentes tableros por cada movimiento asi como su valor de evaluacion
		for i in posibles:
			tableros = self.generarMovimientoSiguiente(i)
			for tablero in tableros:
   				# Evaluamos el valor del tablero y nos quedamos con la mejor puntuacion para realizar movimiento
				puntuacion = evaluar_tablero(self.tablero, jugador)

				if (puntuacion > max_puntuacion):
					max_puntuacion = puntuacion
					mejor_movimiento = i

		self.movimiento(i)


# Evaluamos tablero en funcion de nuestras heuristicas y el cromosoma del jugador
# Las tres primeras heuristicas restan
# Las tres heuristicas siguientes suman
# El resto de heuristicas suman
def evaluar_tablero(tablero, jugador):
	puntuacion = jugador.get_chromosome().get_gen(0).get_numero() * (heuristica1(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(1).get_numero() * (heuristica2(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(2).get_numero() * (heuristica3(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(3).get_numero() * (heuristica4(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(4).get_numero() * (heuristica5(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(5).get_numero() * (heuristica6(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(6).get_numero() * (heuristica7(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(7).get_numero() * (heuristica8(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(8).get_numero() * (heuristica9(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(9).get_numero() * (heuristica10(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(10).get_numero() * (heuristica11(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(11).get_numero() * (heuristica12(tablero, jugador) * 14) / 100
	puntuacion += jugador.get_chromosome().get_gen(12).get_numero() * (heuristica13(tablero, jugador) * 14) / 100

	return puntuacion

def imprimirTablero(tablero):
	print()
	print(' ', end='')
	for x in range(1, MAX_COLUMNAS + 1):
		print(' %s  ' % x, end='')
	print()

	print('+---+' + ('---+' * (MAX_COLUMNAS - 1)))

	for y in range(MAX_FILAS):
		print('|   |' + ('   |' * (MAX_COLUMNAS - 1)))

		print('|', end='')
		for x in range(MAX_COLUMNAS):
			print(' %s |' % tablero.get_tablero()[x][y], end='')
		print()

		print('|   |' + ('   |' * (MAX_COLUMNAS - 1)))

		print('+---+' + ('---+' * (MAX_COLUMNAS - 1)))

# Le damos 1 punto al empate, 3 puntos a la victoria y 0 puntos a la derrota
def jugar(tablero, jugador1, jugador2):
	# Turnos impares para jugador1 y turnos pares para jugador2
	i = 0
	imprimirTablero(tablero)
	while(tablero.tablero_completo() == False):
		# Estamos ante el turno del jugador1
		if(i%2 == 0):
			tablero.realizar_movimiento(jugador1)
			imprimirTablero(tablero)
		# Estamos ante el turno del jugador1
		else:
			tablero.realizar_movimiento(jugador2)
			imprimirTablero(tablero)

		if(tablero.comprobarVictoria() == True):
			if(i%2 == 0):
				jugador1.set_victorias(jugador1.get_victorias() + 3)
				print("Victoria del jugador " + jugador1.get_nombre())
				break
			else:
				jugador2.set_victorias(jugador2.get_victorias() + 3)
				print("Victoria del jugador " + jugador2.get_nombre())
				break


	jugador1.set_victorias(jugador1.get_victorias() + 1)
	jugador2.set_victorias(jugador2.get_victorias() + 1)
	print("Empate")

if __name__ == "__main__":
	i = 0
	contador = 0
	poblacion = Poblacion()
	poblacion.primera_generacion()

	while(i < NUM_POBLACIONES):
		print("Generacion " + str(i))
		# Se realiza el torneo con todos los jugadores de la poblacion enfrentandose al resto de jugadores
		for i in range(len(poblacion.get_jugadores())):
			j = i + 1
			for j in range(len(poblacion.get_jugadores())):
				tablero = Tablero()
				print("Partido" + str(contador) + ": " + poblacion.get_jugador(i).get_nombre() + " VS " + poblacion.get_jugador(j).get_nombre())
				jugar(tablero, poblacion.get_jugador(i), poblacion.get_jugador(j))
				contador += 1

		print("Fin del torneo, estos son los resultados")
		lista = ranking(poblacion)
		for jugador in lista:
			print("Nombre jugador: " + jugador.get_nombre() + " Victorias: " + str(jugador.get_victorias()))

		sleep(5000)