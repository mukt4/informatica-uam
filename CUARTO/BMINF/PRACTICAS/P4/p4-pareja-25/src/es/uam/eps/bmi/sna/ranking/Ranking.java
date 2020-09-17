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
package es.uam.eps.bmi.sna.ranking;

/**
 *
 * @author pablo
 * 
 * Aquí generalizamos las estructuras de ránking para que almacenen cualquier tipo de dato T con score (valor de métrica): usuarios, arcos, etc.
 * El tipo T lo declaramos comparable para deshacer los empates de score con un orden fijo de los elementos.
 */
public interface Ranking<T extends Comparable<T>> extends Iterable<RankingElement<T>> {
    public void add(T element, double score);
    public int size();
    public int totalSize();
}
