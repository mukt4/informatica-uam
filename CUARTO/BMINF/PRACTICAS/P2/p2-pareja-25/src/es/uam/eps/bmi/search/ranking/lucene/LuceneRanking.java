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
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.util.Iterator;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;

/**
 *
 * @author pablo
 * @author javier
 */
public class LuceneRanking implements SearchRanking {
    Iterator<SearchRankingDoc> iterator;
    int size;
    long nResults;
    
    public LuceneRanking (Index index, TopDocs results) {
        ScoreDoc[] ranking = results.scoreDocs;
        size = ranking.length;
        iterator = new LuceneRankingIterator(index, ranking);
        nResults = results.totalHits.value;
    }
    
    // Empty result list
    public LuceneRanking () {
        size = 0;
        iterator = new LuceneRankingIterator();
    }
    
    public Iterator<SearchRankingDoc> iterator() {
        return iterator;
    }

    public int size() {
        return size;
    }

    public long nResults() { return nResults;}
}
