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
package es.uam.eps.bmi.search.ranking.lucene;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.util.Iterator;
import org.apache.lucene.search.ScoreDoc;

/**
 *
 * @author pablo
 */
public class LuceneRankingIterator implements Iterator<SearchRankingDoc> {
    ScoreDoc results[];
    Index index;
    int n = 0;

    public LuceneRankingIterator (Index idx, ScoreDoc r[]) {
        index = idx;
        results = r;
    }
    
    // Empty result list
    public LuceneRankingIterator () {
        index = null;
        results = new ScoreDoc[0];
    }
    
    public boolean hasNext() {
        return n < results.length;
    }

    public SearchRankingDoc next() {
        return new LuceneRankingDoc(index, results[n++]);
    }
}
