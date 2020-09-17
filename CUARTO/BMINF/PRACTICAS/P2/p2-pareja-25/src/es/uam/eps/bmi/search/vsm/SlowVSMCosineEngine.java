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
package es.uam.eps.bmi.search.vsm;

import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.ForwardIndex;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class SlowVSMCosineEngine extends AbstractVSMEngine {

    public SlowVSMCosineEngine(Index index) throws IOException {
        super(index);
    }
    
    public SearchRanking search(String q, int cutoff) throws IOException {
        RankingImpl ranking = new RankingImpl(index, cutoff);
        String query[] = parse(q);
        for (int doc = 0; doc < index.numDocs(); doc++) {
            double score = score(doc, query);
            if (score > Double.NEGATIVE_INFINITY) ranking.add(doc, score);
        }
        return ranking;
    }
    
    double score(int docID, String query[]) throws IOException {
        double s = 0;
        FreqVector docV = ((ForwardIndex) index).getDocVector(docID);
        for (String qWord : query) {
            long freq = docV.getFreq(qWord);
            if (freq > 0) s += tfidf(freq, index.getDocFreq(qWord), index.numDocs());
        }
        return s == 0? Double.NEGATIVE_INFINITY : s/index.getDocNorm(docID);
    }
}
