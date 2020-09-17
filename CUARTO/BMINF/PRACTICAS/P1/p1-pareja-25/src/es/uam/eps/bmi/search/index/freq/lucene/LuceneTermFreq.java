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

import es.uam.eps.bmi.search.index.freq.TermFreq;
import java.io.IOException;
import org.apache.lucene.index.TermsEnum;

/**
 *
 * @author Pablo Castells
 */
public class LuceneTermFreq implements TermFreq{
    TermsEnum terms;
    public LuceneTermFreq (TermsEnum t) {
        terms = t;
    }

    public String getTerm() throws IOException {
        return terms.term().utf8ToString();
    }

    public long getFreq() throws IOException {
        return terms.totalTermFreq();
    }
}
