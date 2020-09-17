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
package es.uam.eps.bmi.sna.metric;

import es.uam.eps.bmi.sna.ranking.Ranking;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

/**
 *
 * @author pablo
 * 
 * Puede aplicarse a usuarios (T representa usuario tipo U) o arcos (T representa arcos tipo Edge<U>).
 */
public interface LocalMetric<T extends Comparable<T>,U> {
    public Ranking<T> compute(UndirectedSocialNetwork<U> network);
    public double compute(UndirectedSocialNetwork<U> network, T element);
}
