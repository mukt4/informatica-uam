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
 */
public interface Ratings {
    public void rate(int user, int item, Double rating);
    public Double getRating(int user, int item);
    public Set<Integer> getUsers(int item);
    public Set<Integer> getItems(int user);
    public Set<Integer> getUsers();
    public Set<Integer> getItems();
    public int nRatings();
    public Ratings[] randomSplit(double ratio);
}
