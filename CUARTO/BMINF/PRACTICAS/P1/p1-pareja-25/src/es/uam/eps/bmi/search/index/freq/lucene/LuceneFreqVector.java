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
package es.uam.eps.bmi.search.index.freq.lucene;

import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.freq.TermFreq;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.Terms;

/**
 *
 * @author Pablo Castells
 *
 */
public class LuceneFreqVector implements FreqVector {
    LuceneFreqVectorIterator iterator;
    
    public LuceneFreqVector(Terms terms) throws IOException {
        iterator = new LuceneFreqVectorIterator(terms);

    }
    public long size() throws IOException {
        return iterator.size;
    }

    public Iterator<TermFreq> iterator() {
        return iterator;
    }
    
    public long getFreq(String term) throws IOException {
        return iterator.getFreq(term);
    }
}
