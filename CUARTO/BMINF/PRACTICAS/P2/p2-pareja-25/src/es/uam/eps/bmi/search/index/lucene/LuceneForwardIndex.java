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
package es.uam.eps.bmi.search.index.lucene;

import es.uam.eps.bmi.search.index.ForwardIndex;
import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.freq.lucene.LuceneFreqVector;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class LuceneForwardIndex extends LuceneIndex implements ForwardIndex {
    public LuceneForwardIndex(String indexFolder) throws IOException {
        super(indexFolder);
    }

    public FreqVector getDocVector(int docID) throws IOException {
        return new LuceneFreqVector(index.getTermVector(docID, "content"));
    }
    
    public long getTermFreq(String term, int docID) throws IOException {
        return getDocVector(docID).getFreq(term);
    }
}
