# Python bytecode 3.5 (3351)
# Embedded file name: p3_utils.py
# Compiled at: 2019-11-11 22:00:59
# Size of source mod 2**32: 28951 bytes
# Decompiled by https://python-decompiler.com
import numpy as np, glob, skimage.data as data, scipy
from skimage.io import imread
from skimage.color import rgb2grey
from skimage.transform import resize
from scipy.spatial.distance import cdist
from collections import Counter
from skimage.feature import hog
from scipy.cluster.vq import kmeans
import cv2 as cv
import os
PRECISION = 2
categories = [
 'Kitchen', 'Store', 'Bedroom', 'LivingRoom', 'Office',
 'Industrial', 'Suburb', 'InsideCity', 'TallBuilding', 'Street',
 'Highway', 'OpenCountry', 'Coast', 'Mountain', 'Forest']
num_train_per_cat = 100

def cls():
    os.system('cls' if os.name == 'nt' else 'clear')


def check_errors_variables(user_out, true_out, check_type=1, check_shape=1, check_dtype=1, check_dataval=1):
    if check_type == 1:
        if not type(user_out) == type(true_out):
            print('\nError! - Resultado tiene tipo {} (se espera tipo {}).'.format(type(user_out), type(true_out)))
            return False
        if check_shape == 1:
            pass
        if not user_out.shape == true_out.shape:
            print('\nError! - Resultado tiene dimensiones {} (se espera dimensiones {}).'.format(user_out.shape, true_out.shape))
            return False
        if check_dtype == 1:
            if not user_out.dtype == true_out.dtype:
                print('\nError! - Resultado tiene dtype {} (se espera dtype {}).'.format(user_out.dtype, true_out.dtype))
                return False
            if check_dataval == 1:
                pass
    if not np.allclose(user_out, true_out, atol=np.float_power(10, -PRECISION)):
        print('\nError! - Resultado tiene valores distintos a los esperados.')
        return False
    return True


def get_tiny_images_true(path_imagenes, tamano=16):
    array_imagenes = np.empty(shape=(len(path_imagenes), tamano ** 2))
    for i, archivo_I in enumerate(path_imagenes):
        imagen = imread(archivo_I)
        imagen = rgb2grey(imagen)
        imagen = imagen.astype(float)
        if imagen.max() > 1:
            imagen = imagen / 255.0
        imagen = resize(imagen, (tamano, tamano), anti_aliasing=True)
        array_imagenes[(i,)] = imagen.reshape((1, -1))

    return array_imagenes


def nearest_neighbor_classify_true(caracteristicas_train, caracteristicas_test, etiquetas_train, k=3):
    predicciones = list()
    etiquetas_train = np.array(etiquetas_train)
    distancias = cdist(caracteristicas_test, caracteristicas_train)
    num_test = distancias.shape[0]
    for i in range(num_test):
        closest_y = []
        distancias_i = distancias[i, :]
        indices_ordenados = np.argsort(distancias_i)
        if k == 1:
            prediccion = etiquetas_train[indices_ordenados[0]]
        else:
            indices_ordenados = indices_ordenados[:k]
            predicciones_aux = etiquetas_train[indices_ordenados]
            unique, pos, counts = np.unique(predicciones_aux, return_inverse=True, return_counts=True)
            prediccion = predicciones_aux[counts.argmax()]
        predicciones.append(prediccion)

    return predicciones


def construir_vocabulario_true(path_imagenes, tamano_vocab, iter=1):
    hog_all = obtener_features_hog_true(path_imagenes)
    hog_all = np.vstack(hog_all)
    print('Realizando clustering con K-Means en funcion de las caracteristicas HOG de tamano: {}'.format(hog_all.shape))
    vocabulario = kmeans(hog_all, tamano_vocab, iter=iter)
    return vocabulario[0]


def obtener_features_hog_true(path_imagenes, celdas_bloque=4, tamano=100):
    todos_hog = list()
    for i, file_i in enumerate(path_imagenes):
        imagen = imread(file_i)
        imagen = rgb2grey(imagen)
        imagen = imagen.astype(float)
        if imagen.max() > 1:
            imagen = imagen / 255.0
        imagen = resize(imagen, (tamano, tamano), anti_aliasing=True)
        hog_features = hog(imagen, orientations=9, pixels_per_cell=(4, 4), cells_per_block=(celdas_bloque, celdas_bloque)).reshape(-1, celdas_bloque * celdas_bloque * 9)
        todos_hog.append(hog_features)

    return todos_hog


def obtener_bags_of_words_true(image_paths, data_path):
    hog_all = obtener_features_hog_true(image_paths)
    vocab = np.load(data_path + '/vocab.npy', allow_pickle=True)
    vocab_size = vocab.shape[0]
    results = list()
    for hog_i in hog_all:
        dist = cdist(hog_i, vocab)
        num_features = dist.shape[0]
        y_pred = list()
        for i in range(num_features):
            closest_y = np.argsort(dist[i])[0]
            y_pred.append(closest_y)

        bincount = np.bincount(np.array(y_pred), minlength=vocab_size)
        results.append(bincount / sum(y_pred))

    return np.vstack(results)


def clasificador_svm_true(caracteristicas_train, caracteristicas_test, etiquetas_train, categories):
    predicciones = list()
    etiquetas_train = np.array(etiquetas_train)
    svm = cv.ml.SVM_create()
    svm.setType(cv.ml.SVM_C_SVC)
    svm.setKernel(cv.ml.SVM_RBF)
    svm.setTermCriteria((cv.TERM_CRITERIA_MAX_ITER, 100, 1e-06))
    svm.train(caracteristicas_train.astype(np.float32), cv.ml.ROW_SAMPLE, etiquetas_train)
    y_predict_indices = svm.predict(caracteristicas_test.astype(np.float32))[1]
    y_predict_indices = y_predict_indices.astype(np.int)
    for i in range(y_predict_indices.shape[0]):
        predicted_class = y_predict_indices[i][0]
        predicciones.append(categories[predicted_class])

    return predicciones


def get_image_paths(data_path, categories, num_train_per_cat):
    """
    This function returns lists containing the file path for each train
    and test image, as well as lists with the label of each train and
    test image. By default both lists will be 1500x1, where each
    entry is a char array (or string).
    """
    num_categories = len(categories)
    train_image_paths = [
     None] * (num_categories * num_train_per_cat)
    test_image_paths = [None] * (num_categories * num_train_per_cat)
    train_labels = [
     None] * (num_categories * num_train_per_cat)
    test_labels = [None] * (num_categories * num_train_per_cat)
    train_labels_indices = [
     None] * (num_categories * num_train_per_cat)
    test_labels_indices = [None] * (num_categories * num_train_per_cat)
    for i, cat in enumerate(categories):
        a = os.path.join(data_path, 'train', cat, '*.jpg')
        images = glob.glob(a)
        assert not len(images) == 0, 'La ruta al dataset es incorrecta.'
        for j in range(num_train_per_cat):
            train_image_paths[i * num_train_per_cat + j] = images[j]
            train_labels[i * num_train_per_cat + j] = cat
            train_labels_indices[i * num_train_per_cat + j] = i

        images = glob.glob(os.path.join(data_path, 'test', cat, '*.jpg'))
        for j in range(num_train_per_cat):
            test_image_paths[i * num_train_per_cat + j] = images[j]
            test_labels[i * num_train_per_cat + j] = cat
            test_labels_indices[i * num_train_per_cat + j] = i

    return (train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices)


def get_accuracy(etiquetas, categorias_predichas):
    etiquetas = np.array(etiquetas)
    categorias_predichas = np.array(categorias_predichas)
    total = etiquetas.shape[0]
    correct = np.sum(etiquetas == categorias_predichas)
    accuracy = correct / total * 100
    return accuracy


def test_p3_tarea1():
    from p3_tarea1 import obtener_tiny_images, clasificador_nearest_neighbor, data_path
    train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices = get_image_paths(data_path, categories, num_train_per_cat)
    print('Dataset correctamente cargado.' + '\n')
    print('Comprobando metodo "Tiny Images" para representacion de las imagenes.')
    train_image_feats_estudiante = obtener_tiny_images(train_image_paths)
    test_image_feats_estudiante = obtener_tiny_images(test_image_paths)
    train_image_feats_true = get_tiny_images_true(train_image_paths)
    test_image_feats_true = get_tiny_images_true(test_image_paths)
    if check_errors_variables(train_image_feats_estudiante, train_image_feats_true) == False or check_errors_variables(test_image_feats_estudiante, test_image_feats_true) == False:
        return False
    print('Comprobando metodo "Tiny Images" para representacion de las imagenes. [CORRECTO]')
    print('Clasificando las imagenes de test.')
    for k in range(1, 4):
        predicted_categories_test_estudiante = clasificador_nearest_neighbor(train_image_feats_estudiante, test_image_feats_estudiante, train_labels, k)
        predicted_categories_test_true = nearest_neighbor_classify_true(train_image_feats_true, test_image_feats_true, train_labels, k)
        accuracy_estudiante = get_accuracy(test_labels, predicted_categories_test_estudiante)
        accuracy_true = get_accuracy(test_labels, predicted_categories_test_true)
        print('-' * 50)
        print('Estudiante    - > Precision obtenida usando representacion "Tiny Images" con classificador K-Nearest-Neighbors(KNN) (k = {k:.1f}): {accuracy:.3f} %.'.format(accuracy=accuracy_estudiante, k=k))
        print('Modelo Basico - > Precision obtenida usando representacion "Tiny Images" con classificador K-Nearest-Neighbors(KNN) (k = {k:.1f}): {accuracy:.3f} %.'.format(accuracy=accuracy_true, k=k))

    return True


def test_p3_tarea2():
    from p3_tarea1 import clasificador_nearest_neighbor
    from p3_tarea2 import obtener_features_hog, construir_vocabulario, obtener_bags_of_words, data_path
    train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices = get_image_paths(data_path, categories, num_train_per_cat)
    print('Dataset correctamente cargado.' + '\n')
    print('Comprobando extraccion de Histograma de Gradientes Orientados (HOG)')
    features_hog_estudiante = obtener_features_hog(train_image_paths[0:5])
    features_hog_true = obtener_features_hog_true(train_image_paths[0:5])
    if check_errors_variables(features_hog_estudiante[0], features_hog_true[0]) == False:
        return False
    print('Comprobando extraccion de Histograma de Gradientes Orientados (HOG). [CORRECTO]' + '\n')
    if not os.path.isfile('./' + data_path + '/vocab.npy'):
        print('No hay un vocabulario previamente calculado.')
        print('Comprobando extraccion de vocabulario para las imagenes del set de entrenamiento.')
        vocabulario_estudiante = construir_vocabulario(train_image_paths, tamano_vocab=200, iter=1)
        np.save('./' + data_path + '/vocab.npy', vocabulario_estudiante, allow_pickle=True)
        print('\n')
    else:
        print('El vocabulario ya esta previamente calculado y guardado. Se cargara al calcular las caracteristicas de cada imagen.' + '\n')
    print('Comprobando metodo "Bag of Words" para representacion de las imagenes')
    train_image_feats_estudiante = obtener_bags_of_words(train_image_paths)
    test_image_feats_estudiante = obtener_bags_of_words(test_image_paths)
    train_image_feats_true = obtener_bags_of_words_true(train_image_paths, data_path)
    test_image_feats_true = obtener_bags_of_words_true(test_image_paths, data_path)
    if check_errors_variables(train_image_feats_estudiante, train_image_feats_true) == False or check_errors_variables(test_image_feats_estudiante, test_image_feats_true) == False:
        return False
    print('Comprobando metodo "Bag of Words" para representacion de las imagenes. [CORRECTO]')
    print('Clasificando las imagenes de test.')
    for k in range(1, 4):
        predicted_categories_test_estudiante = clasificador_nearest_neighbor(train_image_feats_estudiante, test_image_feats_estudiante, train_labels, k)
        predicted_categories_test_true = nearest_neighbor_classify_true(train_image_feats_true, test_image_feats_true, train_labels, k)
        accuracy_estudiante = get_accuracy(test_labels, predicted_categories_test_estudiante)
        accuracy_true = get_accuracy(test_labels, predicted_categories_test_true)
        print('-' * 50)
        print('Estudiante    - > Precision obtenida usando representacion "Bag of Words" con classificador K-Nearest-Neighbors(KNN) (k = {k:.1f}): {accuracy:.3f} %.'.format(accuracy=accuracy_estudiante, k=k))
        print('Modelo Basico - > Precision obtenida usando representacion "Bag of Words" con classificador K-Nearest-Neighbors(KNN) (k = {k:.1f}): {accuracy:.3f} %.'.format(accuracy=accuracy_true, k=k))

    return True


def test_p3_tarea3a():
    from p3_tarea2 import construir_vocabulario, obtener_features_hog, obtener_bags_of_words
    from p3_tarea3a import clasificador_svm, data_path
    train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices = get_image_paths(data_path, categories, num_train_per_cat)
    print('Dataset correctamente cargado.' + '\n')
    print('Comprobando extraccion de Histograma de Gradientes Orientados (HOG)')
    features_hog_estudiante = obtener_features_hog(train_image_paths[0:5])
    features_hog_true = obtener_features_hog_true(train_image_paths[0:5])
    if check_errors_variables(features_hog_estudiante[0], features_hog_true[0]) == False:
        return False
    print('Comprobando extraccion de Histograma de Gradientes Orientados (HOG). [CORRECTO]' + '\n')
    if not os.path.isfile('./' + data_path + '/vocab.npy'):
        print('No hay un vocabulario previamente calculado.')
        print('Comprobando extraccion de vocabulario para las imagenes del set de entrenamiento.')
        vocab_size = 200
        vocabulario_estudiante = construir_vocabulario(train_image_paths, vocab_size, iter=1)
        np.save('./' + data_path + '/vocab.npy', vocabulario_estudiante, allow_pickle=True)
        print('\n')
    else:
        print('El vocabulario ya esta previamente calculado y guardado. Se cargara al calcular las caracteristicas de cada imagen.' + '\n')
    print('Comprobando metodo "Bag of Words" para representacion de las imagenes')
    train_image_feats_estudiante = obtener_bags_of_words(train_image_paths)
    test_image_feats_estudiante = obtener_bags_of_words(test_image_paths)
    train_image_feats_true = obtener_bags_of_words_true(train_image_paths, data_path)
    test_image_feats_true = obtener_bags_of_words_true(test_image_paths, data_path)
    if check_errors_variables(train_image_feats_estudiante, train_image_feats_true) == False or check_errors_variables(test_image_feats_estudiante, test_image_feats_true) == False:
        return False
    print('Comprobando metodo "Bag of Words" para representacion de las imagenes. [CORRECTO]')
    predicted_categories_test_estudiante = clasificador_svm(train_image_feats_estudiante, test_image_feats_estudiante, train_labels_indices, categories)
    predicted_categories_test_true = clasificador_svm_true(train_image_feats_true, test_image_feats_true, train_labels_indices, categories)
    accuracy_estudiante = get_accuracy(test_labels, predicted_categories_test_estudiante)
    accuracy_true = get_accuracy(test_labels, predicted_categories_test_true)
    print('-' * 50)
    print('Estudiante    - > Precision obtenida usando representacion "Bag of Words" con classificador SVM: {accuracy:.3f} %.'.format(accuracy=accuracy_estudiante))
    print('Modelo Basico - > Precision obtenida usando representacion "Bag of Words" con classificador SVM: {accuracy:.3f} %.'.format(accuracy=accuracy_true))
    return True


def test_p3_tarea3b():
    from p3_tarea3b_v3 import construir_vocabulario, obtener_features_hog, obtener_bags_of_words, clasificador_svm, data_path
    train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices = get_image_paths(data_path, categories, num_train_per_cat)
    print('Dataset correctamente cargado.' + '\n')
    print('Extrayendo de Histograma de Gradientes Orientados (HOG)')
    features_hog_estudiante = obtener_features_hog(train_image_paths[0:5])
    if not os.path.isfile('./' + data_path + '/vocab.npy'):
        print('No hay un vocabulario previamente calculado.')
        print('Extrayendo vocabulario para las imagenes del set de entrenamiento.')
        vocab_size = 250
        vocabulario_estudiante = construir_vocabulario(train_image_paths, vocab_size, iter=1)
        np.save('./' + data_path + '/vocab.npy', vocabulario_estudiante, allow_pickle=True)
    else:
        print('El vocabulario ya esta previamente calculado y guardado. Se cargara al calcular las caracteristicas de cada imagen.' + '\n')
    print('Extrayendo "Bag of Words" para representacion de las imagenes')
    train_image_feats_estudiante = obtener_bags_of_words(train_image_paths)
    test_image_feats_estudiante = obtener_bags_of_words(test_image_paths)
    predicted_categories_test_estudiante = clasificador_svm(train_image_feats_estudiante, test_image_feats_estudiante, train_labels_indices, categories)
    accuracy_estudiante = get_accuracy(test_labels, predicted_categories_test_estudiante)
    print('-' * 50)
    print('Estudiante    - > Precision obtenida: {accuracy:.3f} %.'.format(accuracy=accuracy_estudiante))
    print('Modelo Basico - > Precision obtenida: 41.400 %.')
    return True