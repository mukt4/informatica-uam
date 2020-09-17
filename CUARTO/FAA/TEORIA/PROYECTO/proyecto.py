from funciones import construir_vocabulario, obtener_features_hog, obtener_bags_of_words, clasificador_svm, data_path, get_image_paths, get_accuracy
import os
import numpy as np

categories = [
 'A', 'E', 'I', 'O', 'U',
 'a', 'e', 'i', 'o', 'u']
 
num_train_per_cat = 80
num_test_per_cat = 30

if __name__ == '__main__':
    train_image_paths, test_image_paths, train_labels, test_labels, train_labels_indices, test_labels_indices = get_image_paths(data_path, categories, num_train_per_cat, num_test_per_cat)
    print('Dataset correctamente cargado.' + '\n')
    if not os.path.isfile('./' + data_path + '/vocab.npy'):
        print('No hay un vocabulario previamente calculado.')
        print('Extrayendo vocabulario para las imagenes del set de entrenamiento.')
        vocab_size = 250
        vocabulario = construir_vocabulario(train_image_paths, vocab_size, iter=1)
        np.save('./' + data_path + '/vocab.npy', vocabulario, allow_pickle=True)
    else:
        print('El vocabulario ya esta previamente calculado y guardado. Se cargara al calcular las caracteristicas de cada imagen.' + '\n')
    print('Extrayendo "Bag of Words" para representacion de las imagenes')
    train_image_feats = obtener_bags_of_words(train_image_paths)
    test_image_feats = obtener_bags_of_words(test_image_paths)
    predicted_categories_test = clasificador_svm(train_image_feats, test_image_feats, train_labels_indices, categories)
    accuracy = get_accuracy(test_labels, predicted_categories_test)
    print('-' * 50)
    print('Conclusion    - > Precision obtenida: {accuracy:.3f} %.'.format(accuracy=accuracy))