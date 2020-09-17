# -*- coding: utf-8 -*-
from Datos import Datos
from itertools import *
import numpy as np
import collections
import math
import warnings
import random


# Función que devuelve A intervalos en los que se dividirá la columna.
# Empleando la propuesta de Sturges
def get_intervalo(att_col):

	N = att_col.shape[0]

	Xmax = np.amax(att_col)
	Xmin = np.amin(att_col)

	K = int(1 + 3.322*math.log(N,10))
	A = (Xmax - Xmin)/K

	intervalo = {}
	for i in range(1,int(K)+1):
		intervalo[i] = [Xmin, Xmin + A]
		Xmin = Xmin + A


	return intervalo,K


def create_att_diccionary(dataset):

	list_tabla_intervalos = []
	n_intervalos = []
	dataset = np.array(dataset)
	for i in range(dataset.shape[1]-1):
		intervalo,k = get_intervalo(dataset[:,i])
		list_tabla_intervalos.append(intervalo)
		n_intervalos.append(k)

	n_intervalos.append(len(np.unique(dataset[:,-1])))
	return list_tabla_intervalos,n_intervalos


def get_k(dic_intervalos):
	 return int(np.amax(list(dic_intervalos.keys())))


def get_i_interval(dic_intervalos, att):

	for key in dic_intervalos:
		imin,imax = dic_intervalos.get(key)

		if imin < att < imax:
			return key

	return 0



# Devuelve un iterador que itera los elementos de 2 en 2
# Sirve para el cruce, poder coger progenitores 2 a 2.
def iter_in_pair(iterable):
	a,b = tee(iterable)
	next(b,None)
	return zip(a,b)
