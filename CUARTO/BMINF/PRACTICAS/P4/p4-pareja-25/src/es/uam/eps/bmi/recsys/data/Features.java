/*
 *  Copyright (C) 2020 Pablo Castells y Javier Sanz-Cruzado
 *
 *  Este código se ha implementado para la realización de las prácticas de
 *  la asignatura "Búsqueda y minería de información" de 4º del Grado en
 *  Ingeniería Informática, impartido en la Escuela Politécnica Superior de
 *  la Universidad Autónoma de Madrid. El fin del mismo, así como su uso,
 *  se ciñe a las actividades docentes de dicha asignatura.
 *
 */
package es.uam.eps.bmi.recsys.data;

import java.util.Set;

/**
 *
 * @author pablo
 * 
 * En principio características de ítems, pero también podrían ser de usuario.
 */
public interface Features<F> {
    public Set<F> getFeatures(int id);
    public Double getFeature(int id, F feature);
    public void setFeature(int id, F feature, double value);
    public Set<Integer> getIDs();
}
