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

import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;

/**
 *
 * @author pablo
 * 
 * Implementación de heap de ranking. Vale tanto para resultados de una recomendación, como para ordenar y seleccionar k vecinos más similares.
 */
public class RankingImpl implements Ranking {
    PriorityQueue<RankingElement> rankingHeap;
    int cutoff;
    int nElements;
    
    public RankingImpl (int n) {
        cutoff = n;
        nElements = 0;
        rankingHeap = new PriorityQueue<RankingElement>(Comparator.reverseOrder());
    }
    
    public void add(int id, double score) {
        rankingHeap.add(new RankingElementImpl(id,score));
        if (rankingHeap.size() > cutoff) rankingHeap.poll();
        nElements++;
    }
    
    public Iterator<RankingElement> iterator() {
        return new RankingIteratorImpl(rankingHeap);
    }

    public int size() {
        return rankingHeap.size();
    }

    public int totalSize() {
        return nElements;
    }
}
