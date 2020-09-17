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
package es.uam.eps.bmi.recsys.ranking;

/**
 *
 * @author pablo
 * 
 * En esta práctica esta interfaz sirve para recomendaciones, pero también para ránkings de los k vecinos más similares.
 */
public interface Ranking extends Iterable<RankingElement> {
    public void add(int element, double score);
    public int size();
    public int totalSize();
}
