from sklearn import datasets, linear_model
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score
import numpy as np
from sklearn import preprocessing


# Funcion que realiza el algoritmo de vecinos proximos utlizando las librerias de 
# Scikit Learn
# Parametros de entrada:
#   - dataset (Conjunto de datos)
#   - particiones (Array que contiene los indices de las particiones para los datos)
#   - k (Numero de vecinos proximos para el algoritmo)
def Knn_Scikit(dataset, particiones, k):
    atributos = preprocessing.OneHotEncoder(categorical_features=dataset.nominalAtributos[:-1],sparse=False)
    x = atributos.fit_transform(dataset.datos[:,:-1])
    y = dataset.datos[:,-1]
    table = "<table><tr> <th>K vecinos</th> <th>Error</th></tr>"

    for i in k:
        clf = KNeighborsClassifier(n_neighbors=i, p=2, metric="euclidean")
        aciertos = cross_val_score(clf, x, y, cv=10, n_jobs=-1)
        mediaError = 1-aciertos.mean()
        errorStd = aciertos.std()
        errores = []
        
        for a in aciertos:
            errores.append(1 - a)
        table += "<tr> <td>" + str(i)
        table += "</td><td>%.4f  ±%4.f</td></tr>" % (np.mean(mediaError), np.std(errorStd))

    table += "</table>"
    return table


# Funcion que realiza el algoritmo de regresion logistica utlizando las librerias de 
# Scikit Learn
# Parametros de entrada:
#   - dataset (Conjunto de datos)
#   - particiones (Array que contiene los indices de las particiones para los datos)
#   - n_epocas (Numero de epocas del algoritmo)
def RegLog_Scikit(dataset, particiones, n_epocas):
    atributos = preprocessing.OneHotEncoder(categorical_features=dataset.nominalAtributos[:-1],sparse=False)
    x = atributos.fit_transform(dataset.datos[:,:-1])
    y = dataset.datos[:,-1]
    table = "<table><tr> <th>N epocas</th> <th>Error</th></tr>"

    for i in n_epocas:
        clf = LogisticRegression(max_iter=i)
        aciertos = cross_val_score(clf, x, y, cv=10, n_jobs=-1)
        mediaError = 1-aciertos.mean()
        errorStd = aciertos.std()
        errores = []
        
        for a in aciertos:
            errores.append(1 - a)
        table += "<tr> <td>" + str(i)
        table += "</td><td>%.4f  ±%4.f</td></tr>" % (np.mean(mediaError), np.std(errorStd))

    table += "</table>"
    return table