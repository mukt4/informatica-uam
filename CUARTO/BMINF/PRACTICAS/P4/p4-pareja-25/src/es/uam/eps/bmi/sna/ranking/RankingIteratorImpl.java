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

import java.util.Iterator;
import java.util.PriorityQueue;

/**
 *
 * @author pablo
 */
public class RankingIteratorImpl<T extends Comparable<T>> implements Iterator<RankingElement<T>> {
    RankingElement<T> ranking[];
    int pos;

    public RankingIteratorImpl (PriorityQueue<RankingElement<T>> r) {
        PriorityQueue<RankingElement<T>> results = new PriorityQueue<RankingElement<T>>(r);
        ranking = new RankingElement[results.size()];
        pos = ranking.length;
        while (!results.isEmpty())
            ranking[--pos] = results.poll();
    }
    
    public boolean hasNext() {
        return pos < ranking.length;
    }

    public RankingElement<T> next() {
         return ranking[pos++];
    }
}
