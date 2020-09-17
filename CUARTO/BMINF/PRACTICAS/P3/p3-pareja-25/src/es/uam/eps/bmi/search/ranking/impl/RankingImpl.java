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
package es.uam.eps.bmi.search.ranking.impl;

import es.uam.eps.bmi.search.index.DocumentMap;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;

/**
 *
 * @author pablo
 * @author javier
 */
public class RankingImpl implements SearchRanking {
    DocumentMap map;
    PriorityQueue<SearchRankingDoc> rankingHeap;
    int cutoff;
    long nResults;

    public RankingImpl (DocumentMap map, int n) {
        this.map = map;
        cutoff = n;
        nResults = 0;
        rankingHeap = new PriorityQueue<SearchRankingDoc>(Comparator.reverseOrder());
    }

    public void add(int docID, double score) {
        if (rankingHeap.size() < cutoff || score > rankingHeap.peek().getScore()) {
            if (rankingHeap.size() == cutoff) rankingHeap.poll();
            rankingHeap.add(new RankingDocImpl(map, docID, score));
        }
        nResults++;
    }

    public Iterator<SearchRankingDoc> iterator() {
        return new RankingIteratorImpl(rankingHeap);
    }

    public int size() {
        return rankingHeap.size();
    }

    public long nResults() {
        return nResults;
    }
}
