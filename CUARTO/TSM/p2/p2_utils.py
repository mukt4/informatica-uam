# uncompyle6 version 3.5.0
# Python bytecode 3.5 (3351)
# Decompiled from: Python 3.6.8 (default, Oct  9 2019, 14:04:01) 
# [GCC 5.4.0 20160609]
# Embedded file name: d:\OneDrive - Universidad Autonoma de Madrid\docencia\grado.gitst.tsv\practicas\code\practicasTSV\p2_sol\p2_utils.py
# Compiled at: 2019-11-08 08:19:43
# Size of source mod 2**32: 38492 bytes
import numpy as np, os
PRECISION = 2

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


def print_error_listas(user_out, true_out, tipo='esquinas'):
    user_out = np.asarray(user_out)
    print('\n{} verdadero ({}):'.format(tipo, len(true_out)), flush=True)
    for data in true_out:
        print(data, end=' ', flush=True)

    print('\n\n{} estudiante ({}):'.format(tipo, len(user_out)), flush=True)
    for data in user_out:
        print(data, end=' ', flush=True)

    if len(user_out) > 0:
        list_tp = [data for data in user_out if -data[0] * data[1] in -true_out[:, 0] * true_out[:, 1]]
        print('\n\n{} correctas ({}/{}):'.format(tipo, len(list_tp), len(user_out)), flush=True)
        for data in list_tp:
            print(data, end=' ', flush=True)

    else:
        print('\n\n{} correctas ({}/{}):'.format(tipo, 0, len(user_out)), flush=True)
    if len(user_out) > 0:
        list_fp = [data for data in user_out if -data[0] * data[1] not in -true_out[:, 0] * true_out[:, 1]]
        print('\n\n{} erroneas ({}/{}):'.format(tipo, len(list_fp), len(user_out)), flush=True)
        for data in list_fp:
            print(data, end=' ', flush=True)

    else:
        print('\n\n{} erroneas ({}/{}):'.format(tipo, 0, len(user_out)), flush=True)
    if len(user_out) > 0:
        list_tn = [data for data in true_out if -data[0] * data[1] not in -user_out[:, 0] * user_out[:, 1]]
        print('\n\n{} sin detectar ({}/{}):'.format(tipo, len(list_tn), len(true_out)), flush=True)
        for data in list_tn:
            print(data, end=' ', flush=True)

    else:
        print('\n\n{} sin detectar ({}/{}):'.format(tipo, len(true_out), len(true_out)), flush=True)
        for data in true_out:
            print(data, end=' ', flush=True)


def create_checkerboard_testimage(gridsize=8, cellsize=10):
    if gridsize % 2 != 0:
        print('Changing gridsize to ' + str(2 * np.round(gridsize / 2)))
    n = int(np.round(gridsize / 2))
    cell = np.ones((cellsize, cellsize))
    testimage = np.kron([[1, 0] * n, [0, 1] * n] * n, cell)
    return testimage


def get_checkerboard_corners_harris(gridsize=8, cellsize=10):
    if gridsize % 2 != 0:
        print('Changing gridsize to ' + str(2 * np.round(gridsize / 2)))
    n = int(np.round(gridsize / 2))
    cornerpoints = np.empty([(2 * n - 1) * (2 * n - 1), 2], dtype=np.int64)
    x = 0
    for d1 in range(2 * n - 1):
        for d2 in range(2 * n - 1):
            p1 = cellsize - 1 + d1 * cellsize
            p2 = cellsize - 1 + d2 * cellsize
            cornerpoints[(x, 0)] = p1
            cornerpoints[(x, 1)] = p2
            x = x + 1

    return cornerpoints


def detectar_puntos_interes_harris_true(imagen, sigma=1, k=0.05):
    from skimage.feature import corner_harris, corner_peaks
    harris_response = corner_harris(imagen, method='k', k=k, sigma=sigma)
    coords_esquinas = corner_peaks(harris_response, min_distance=5, threshold_abs=None, threshold_rel=0.2, exclude_border=True, indices=True)
    return coords_esquinas


def descripcion_puntos_interes_true(imagen, coords_esquinas, vtam=16, nbins=16, tipoDesc='hist'):
    import scipy
    imagen = imagen.astype(float)
    if imagen.max() > 1:
        imagen = imagen / 255.0
    n_puntos_interes = coords_esquinas.shape[0]
    data = []
    data_grad = []
    data_mag = []
    data_ori = []
    descriptores = []
    new_coords_esquinas = []
    parche_grad = []
    parche_mag = []
    parche_theta = []
    if tipoDesc.lower() == 'mag-ori':
        dy = scipy.ndimage.sobel(imagen, axis=0, mode='constant')
        dx = scipy.ndimage.sobel(imagen, axis=1, mode='constant')
        mag = np.sqrt(dy ** 2 + dx ** 2)
        theta = np.rad2deg(np.arctan2(dx, dy))
        theta[theta < 0] = theta[(theta < 0)] + 360
    total  = 0
    for i in range(n_puntos_interes):
        y = coords_esquinas[(i, 0)]
        x = coords_esquinas[(i, 1)]
        offset = vtam // 2
        if x - offset >= 0 and x + offset < imagen.shape[1] and y - offset >= 0 and y + offset < imagen.shape[0]:
            parche = imagen[y - offset:y + offset + 1, x - offset:x + offset + 1]
            if tipoDesc.lower() == 'hist':
                desc, bins_hist = np.histogram(parche.astype(float).flatten(), nbins, range=(0,
                                                                                             1))
            elif tipoDesc.lower() == 'mag-ori':
                parche_grad = [
                 dy[y - offset:y + offset + 1, x - offset:x + offset + 1],
                 dx[y - offset:y + offset + 1, x - offset:x + offset + 1]]
                parche_mag = mag[y - offset:y + offset + 1, x - offset:x + offset + 1]
                parche_theta = theta[y - offset:y + offset + 1, x - offset:x + offset + 1]
                bins_hist = np.linspace(start=0, stop=360, num=nbins + 1, endpoint=True)
                parche_theta_bins = np.digitize(parche_theta, bins_hist)
                desc = np.zeros(nbins)
                for bin_i in range(1, nbins + 1):
                    mask = np.array(parche_theta_bins == bin_i).flatten()
                    desc[bin_i - 1] = np.sum(parche_mag.flatten()[mask])
            else:
                desc = []
            data.append(parche)
            data_grad.append(parche_grad)
            data_mag.append(parche_mag)
            data_ori.append(parche_theta)
            descriptores.append(desc)
            new_coords_esquinas.append([y, x])

    return (
     np.asarray(descriptores), bins_hist, np.asarray(new_coords_esquinas), np.asarray(data), np.asarray(data_grad), np.asarray(data_mag), np.asarray(data_ori))


def correspondencias_puntos_interes_true(descriptores_imagen1, descriptores_imagen2, tipoDist='mindist', max_distancia=25, max_nndr=0.75):
    """from skimage.feature import match_descriptors

    if tipoDist.lower() == 'mindist':
        #distancia minima
        matches = match_descriptors(desc1, desc2, metric='euclidean',
                                                  max_distance=max_distancia, # umbral distancia minima
                                                  cross_check=True)          # sin verificar asignaciones repetidas
       # Nearest Neighbor Distance Ratio, 
    elif tipoDist.lower() == 'nndr':
        matches = match_descriptors(desc1, desc2, metric='euclidean',
                                                  max_distance=max_distancia, # umbral distancia minima
                                                  cross_check=True,          # sin verificar asignaciones repetidas
                                                  max_ratio = max_nndr)       # umbral ratio nndr
    return matches
    """
    from scipy.spatial.distance import cdist
    correspondencias = []
    if tipoDist.lower() == 'mindist':
        desc_selected = np.full(descriptores_imagen2.shape[0], False, dtype=bool)
        for i, desc1 in enumerate(descriptores_imagen1):
            mindist = 10000000000.0
            mindist_id = -1
            for j, desc2 in enumerate(descriptores_imagen2):
                dist_euclidea = np.linalg.norm(desc1 - desc2)
                if dist_euclidea < mindist and desc_selected[j] == False:
                    mindist = dist_euclidea
                    mindist_id = j

            if mindist < max_distancia:
                correspondencias.append([i, mindist_id])
                desc_selected[mindist_id] = True

    elif tipoDist.lower() == 'nndr':
        distances = cdist(descriptores_imagen1, descriptores_imagen2, metric='euclidean')
        indices1 = np.arange(descriptores_imagen1.shape[0])
        indices2 = np.argmin(distances, axis=1)
        mask = distances[(indices1, indices2)] < max_distancia
        indices1 = indices1[mask]
        indices2 = indices2[mask]
        unique, unique_idx = np.unique(indices2, return_index=True)
        mask = np.full(indices2.shape, False, dtype=bool)
        mask[unique_idx] = True
        indices1 = indices1[mask]
        indices2 = indices2[mask]
        best_distances = distances[(indices1, indices2)]
        distances[(indices1, indices2)] = np.inf
        second_best_indices2 = np.argmin(distances[indices1], axis=1)
        second_best_distances = distances[(indices1, second_best_indices2)]
        second_best_distances[second_best_distances == 0] = np.finfo(np.double).eps
        nndr = best_distances / second_best_distances
        mask = nndr < max_nndr
        indices1 = indices1[mask]
        indices2 = indices2[mask]
        correspondencias = np.column_stack((indices1, indices2))
    else:
        print('Error al seleccionar el modo de correspondencias')
    correspondencias = np.asarray(correspondencias)
    correspondencias = correspondencias.astype(np.int64)
    return correspondencias


def get_p2_test_images(set=0):
    import skimage.data as data
    listnames = []
    listimg = []
    if set == 0 or set == 1:
        gridsizes = np.array([4, 8, 12])
        cellsizes = np.array([10, 20])
        for csize in cellsizes:
            for gsize in gridsizes:
                listimg.append(create_checkerboard_testimage(gsize, csize))
                listnames.append('Tablero_' + str(gsize) + 'x' + str(gsize) + '_' + str(csize))

    if set == 0 or set == 2:
        listnames = listnames + ['Cameraman', 'Astronaut', 'Cofee', 'Rocket']
        listimg = listimg + [data.camera(), data.astronaut(), data.coffee(), data.rocket()]
    return (
     listimg, listnames)


def test_p2_tarea1(disptime=-1, set=0):
    import matplotlib.pyplot as plt
    from skimage.color import rgb2gray
    from p2_tarea1 import detectar_puntos_interes_harris
    listimg, listnames = get_p2_test_images(set=set)
    print('Funcion detectar_puntos_interes_harris')
    for img, name in zip(listimg, listnames):
        print('\tTesteando imagen {} con tamano {} ... '.format(name, img.shape), end='', flush=True)
        testimg = rgb2gray(img)
        true_out = detectar_puntos_interes_harris_true(imagen=testimg)
        user_out = detectar_puntos_interes_harris(imagen=np.copy(testimg))
        res = check_errors_variables(user_out=user_out, true_out=true_out)
        if disptime >= 0 or res == False:
            fig = plt.figure(num=100001)
            fig.canvas.set_window_title('Practica 2 - Parte 1 - Comparativa puntos de interes con Harris')
            fig.set_size_inches(9, 8)
            plt.clf()
            plt.imshow(testimg, cmap='gray')
            plt.plot(true_out[:, 1], true_out[:, 0], '+', linewidth=5, color='green')
            if len(user_out) > 0:
                plt.plot(user_out[:, 1], user_out[:, 0], 'x', linewidth=5, color='red')
            plt.legend(['Esquinas a detectar (verdadero)', 'Esquinas detectadas (estudiante)'])
            plt.title('Resultados imagen ' + name)
            if res == True:
                if disptime == 0:
                    plt.show(block=True)
                if disptime > 0:
                    plt.pause(disptime)
            else:
                print('ERROR al determinar esquinas !!!!')
                print_error_listas(user_out, true_out, tipo='Esquinas')
                plt.show(block=True)
                return False
        print('OK.')

    return True


def test_p2_tarea2(disptime=-1, tipoDesc='hist', set=0):
    import matplotlib.pyplot as plt, matplotlib.patches as patches
    from skimage.color import rgb2gray
    from p2_tarea2 import descripcion_puntos_interes
    listimg, listnames = get_p2_test_images(set=set)
    print('Funcion descripcion_puntos_interes')
    for img, name in zip(listimg, listnames):
        vtamList = np.array([8, 16, 32])
        nbinsList = np.array([16, 32])
        for nbins in nbinsList:
            for vtam in vtamList:
                print('\tTesteando descriptores tipo {} con imagen {} {}x{}, vecindario {}x{} y {} bins... '.format(tipoDesc.upper(), name, img.shape[0], img.shape[1], vtam + 1, vtam + 1, nbins), end='', flush=True)
                if len(img.shape) == 3:
                    testimg = rgb2gray(img)
                else:
                    testimg = img
                esquinas = detectar_puntos_interes_harris_true(testimg)
                true_out, bins, true_new_coords, patch_data, patch_grad, patch_mag, patch_ori = descripcion_puntos_interes_true(imagen=testimg, coords_esquinas=esquinas, vtam=vtam, nbins=nbins, tipoDesc=tipoDesc)
                user_out, user_new_coords = descripcion_puntos_interes(imagen=np.copy(testimg), coords_esquinas=np.copy(esquinas), vtam=np.copy(vtam), nbins=np.copy(nbins), tipoDesc=tipoDesc)
                if check_errors_variables(user_out=user_out, true_out=true_out, check_dataval=-1) == False:
                    print('ERROR al determinar descriptores !!!!')
                    print('\nVerifique los intervalos de cuantificacion para {} bins: {}'.format(nbins, bins))
                    print_error_listas(user_new_coords, true_new_coords, tipo='Esquinas')
                    return False
                alldata = zip(user_out, true_out, true_new_coords, patch_data, patch_grad, patch_mag, patch_ori)
                for desc_user, desc_true, esquina, data_gray, data_grad, data_mag, data_ori in alldata:
                    res = check_errors_variables(user_out=desc_user, true_out=desc_true)
                    if disptime >= 0 or res == False:
                        fig = plt.figure(num=100002)
                        fig.canvas.set_window_title('Practica 2 - Parte 2 - Comparativa descriptores {} con vecindario {}x{} y {} bins en el histograma (resto de parametros con valores por defecto)'.format(tipoDesc.upper(), vtam + 1, vtam + 1, nbins))
                        plt.clf()
                        y = esquina[0]
                        x = esquina[1]
                        rect = patches.Rectangle((x - (vtam + 1) // 2, y - (vtam + 1) // 2), vtam + 1, vtam + 1, linewidth=1, edgecolor='g', facecolor='none')
                        if tipoDesc.lower() == 'hist':
                            fig.set_size_inches(10, 7)
                            ax = plt.subplot(2, 2, 1)
                            plt.imshow(testimg, cmap='gray')
                            plt.plot(x, y, 'x', linewidth=5, color='red')
                            ax.add_patch(rect)
                            plt.legend(['Esquina', 'Vecindario'])
                            plt.title('Resultados imagen')
                            plt.subplot(2, 2, 2)
                            plt.imshow(data_gray, cmap='gray')
                            plt.colorbar()
                            plt.title('{}x{} pixeles vecindario'.format(vtam + 1, vtam + 1))
                            plt.subplot(2, 2, 3)
                            plt.hist(bins[:-1], bins, weights=desc_user)
                            plt.title('Descriptor {} bins (estudiante)'.format(nbins))
                            plt.xlabel('Bins (niveles gris)')
                            plt.ylabel('Valor')
                            plt.subplot(2, 2, 4)
                            plt.hist(bins[:-1], bins, weights=desc_true)
                            plt.title('Descriptor {} bins (verdadero)'.format(nbins))
                            plt.xlabel('Bins (niveles gris)')
                            plt.ylabel('Valor')
                        elif tipoDesc.lower() == 'mag-ori':
                            fig.set_size_inches(14, 8)
                            plt.rcParams.update({'font.size': 7})
                            ax = plt.subplot(2, 4, 1)
                            plt.imshow(testimg, cmap='gray')
                            plt.plot(x, y, 'x', linewidth=5, color='red')
                            ax.add_patch(rect)
                            plt.legend(['Esquina', 'Vecindario'])
                            plt.title('Resultados imagen')
                            plt.subplot(2, 4, 2)
                            plt.imshow(data_gray, cmap='gray')
                            plt.colorbar()
                            plt.title('{}x{} vecindario'.format(vtam + 1, vtam + 1))
                            plt.subplot(2, 4, 3)
                            plt.hist(bins[:-1], bins, weights=desc_user)
                            plt.title('Descriptor {} bins \n(estudiante)'.format(nbins))
                            plt.xlabel('Bins (grados)')
                            plt.ylabel('Valor')
                            plt.subplot(2, 4, 4)
                            plt.hist(bins[:-1], bins, weights=desc_true)
                            plt.title('Descriptor {} bins \n(verdadero)'.format(nbins))
                            plt.xlabel('Bins (grados)')
                            plt.ylabel('Valor')
                            plt.subplot(2, 4, 5)
                            plt.imshow(data_grad[0])
                            plt.colorbar()
                            plt.title('Derivada horizontal')
                            plt.subplot(2, 4, 6)
                            plt.imshow(data_grad[1])
                            plt.colorbar()
                            plt.title('Derivada vertical')
                            plt.subplot(2, 4, 7)
                            plt.imshow(data_mag)
                            plt.colorbar()
                            plt.title('Magnitud gradiente')
                            plt.subplot(2, 4, 8)
                            plt.imshow(data_ori)
                            plt.colorbar()
                            plt.title('Orientacion gradiente')
                        else:
                            print('ERROR al pintar. Descriptor desconocido!!!!')
                        plt.draw()
                    if res == True:
                        if disptime == 0:
                            plt.show(block=True)
                        if disptime > 0:
                            plt.pause(disptime)
                    else:
                        print('ERROR al determinar descriptores !!!!')
                        print('Datos utilizados por la funcion de test:')
                        print('- Esquina:{}'.format(esquina))
                        print('- Parche pixeles niveles de gris (ventana):\n{}\n'.format(data_gray))
                        if tipoDesc.lower() == 'mag-ori':
                            print('- Derivada horizontal (ventana):\n{}\n'.format(data_grad[0]))
                            print('- Derivada vertical (ventana):\n{}\n'.format(data_grad[1]))
                            print('- Magnitud (ventana):\n{}\n'.format(data_mag))
                            print('- Orientaciones (ventana):\n{}\n'.format(data_ori))
                        print('- Intervalos de cuantificacion para {} bins ={}'.format(nbins, bins))
                        print('- Descriptor verdadero:{}\n'.format(desc_true))
                        print('Descriptor estudiante:{}\n'.format(desc_user))
                        print('ERROR al determinar descriptores !!!! \nPor favor comprueba que los datos mostrados coinciden con los utilizados en su programa.')
                        plt.show(block=True)
                        return False

                print('OK')

    return True


def test_p2_tarea3(disptime=-1, tipoDesc='hist', tipoDist='mindist', set=2):
    import matplotlib.pyplot as plt, matplotlib.patches as patches
    from skimage.color import rgb2gray
    from skimage.feature import plot_matches
    from skimage import transform as tf
    from p2_tarea3 import correspondencias_puntos_interes
    maxDist = 25
    listimg, listnames = get_p2_test_images(set=set)
    imagen1 = np.zeros((100, 100, 3))
    imagen1[20:50, 20:50, :] = 1
    listimg = [imagen1] + listimg
    listnames = ['test_basica'] + listnames
    tformList = [
     tf.AffineTransform(translation=(0, -60)),
     tf.AffineTransform(translation=(0, -50), scale=(1.2, 1.2)),
     tf.AffineTransform(translation=(0, -25), rotation=25)]
    print('Funcion correspondencias_puntos_interes')
    for image1, name in zip(listimg, listnames):
        if len(image1.shape) == 3:
            img1 = rgb2gray(image1)
        else:
            img1 = image1
        coords1 = detectar_puntos_interes_harris_true(imagen=img1)
        desc1, bins, new_coords1, patch_data, patch_grad, patch_mag, patch_ori = descripcion_puntos_interes_true(imagen=img1, coords_esquinas=coords1, tipoDesc=tipoDesc)
        for i, tform in enumerate(tformList):
            print('\tTesteando correspondencias descriptores tipo {} y distancia {} para imagen {} transformada {}... '.format(tipoDesc.upper(), tipoDist.upper(), name, i), end='', flush=True)
            image2 = tf.warp(image1, tform)
            if len(image2.shape) == 3:
                img2 = rgb2gray(image2)
            else:
                img2 = image2
            coords2 = detectar_puntos_interes_harris_true(imagen=img2)
            desc2, bins2, new_coords2, patch_data2, patch_grad2, patch_mag2, patch_ori2 = descripcion_puntos_interes_true(imagen=img2, coords_esquinas=coords2, tipoDesc=tipoDesc)
            user_out = correspondencias_puntos_interes(descriptores_imagen1=np.copy(desc1), descriptores_imagen2=np.copy(desc2), tipoDist=tipoDist, max_distancia=np.copy(maxDist))
            true_out = correspondencias_puntos_interes_true(descriptores_imagen1=desc1, descriptores_imagen2=desc2, tipoDist=tipoDist, max_distancia=maxDist)
            res = check_errors_variables(user_out, true_out)
            if disptime >= 0 or res == False:
                fig = plt.figure(num=100003)
                plt.clf()
                fig, ax = plt.subplots(num=100003, nrows=2, ncols=1)
                fig.canvas.set_window_title('Practica 2 - Parte 3 - Comparativa descriptores {} y distancias {} (resto de parametros por defecto)'.format(tipoDesc.upper(), tipoDist.upper()))
                plt.gray()
                plot_matches(ax[0], image1, image2, coords1, coords2, user_out)
                ax[0].axis('off')
                ax[0].set_title('Imagen {}: original vs. transformada (resultado estudiante)'.format(name))
                plot_matches(ax[1], image1, image2, coords1, coords2, true_out)
                ax[1].axis('off')
                ax[1].set_title('Imagen {}: original vs. transformada (resultado verdadero)'.format(name))
                plt.draw()
            if res == True:
                if disptime == 0:
                    plt.show(block=True)
                if disptime > 0:
                    plt.pause(disptime)
            else:
                print('ERROR al determinar correspondencias !!!!')
                print('\nEsquinas imagen 1 (verdadero):'.format(new_coords1), flush=True)
                for data in new_coords1:
                    print(data, end=' ', flush=True)

                print('\nEsquinas imagen 2 (verdadero):'.format(new_coords2), flush=True)
                for data in new_coords1:
                    print(data, end=' ', flush=True)

                print('\nDescriptores imagen 1 (verdadero):\n{}\n'.format(desc1))
                print('\nDescriptores imagen 2 (verdadero) :\n{}\n'.format(desc2))
                print_error_listas(user_out, true_out, tipo='Correspondencias')
                plt.show(block=True)
                return False
            print('OK')

    return True
# okay decompiling p2_utils.pyc
