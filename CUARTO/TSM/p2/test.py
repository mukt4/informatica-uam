import numpy as np
import collections

distancias = [[4,5,6,7], [7,2,3,1], [5,6,4,3], [1,2,9,0]]
max_distancia = 6

distancias_validas = []
list_dict = []

#for lista in distancias:
#	dict_distancia = {lista[i] : i for i in range(0, len(lista))}
#	ordenado = collections.OrderedDict(sorted(dict_distancia.items()))
for i, lista in enumerate(distancias):
	for j, distancia in enumerate(lista):
		if distancia < max_distancia:
			distancias_validas.append((i, j, distancia))

print(distancias)
print(distancias_validas)