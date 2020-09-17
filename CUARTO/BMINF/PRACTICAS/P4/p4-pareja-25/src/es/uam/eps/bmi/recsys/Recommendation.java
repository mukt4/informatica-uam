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
package es.uam.eps.bmi.recsys;

import es.uam.eps.bmi.recsys.ranking.Ranking;
import java.io.PrintStream;
import java.util.Set;

/**
 *
 * @author pablo
 */
public interface Recommendation {
    public Set<Integer> getUsers();
    public Ranking getRecommendation(int user);
    public void add(int user, Ranking ranking);
    public void print(PrintStream out);
    public void print(PrintStream out, int userCutoff, int itemCutoff);
}
