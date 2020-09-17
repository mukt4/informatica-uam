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
package es.uam.eps.bmi.search.ranking;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public abstract class SearchRankingDoc implements Comparable<SearchRankingDoc> {
    public abstract double getScore();
    public abstract int getDocID();
    public abstract String getPath() throws IOException;
    
    public int compareTo(SearchRankingDoc d) {
        int comp = (int) Math.signum(d.getScore() - getScore());
        try { // En caso de empate en score, desempatamos por nombre de doc
            comp = comp == 0? getPath().compareTo(d.getPath()) : comp;
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return comp;
    }
}
