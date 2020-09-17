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
package es.uam.eps.bmi.search.index;

import es.uam.eps.bmi.search.index.structure.PostingsList;
import java.io.IOException;
import java.util.Collection;

/**
 *
 * @author pablo
 */
public interface Index {
    public int numDocs();
    public PostingsList getPostings(String term) throws IOException;
    public Collection<String> getAllTerms() throws IOException;
    public long getTotalFreq(String term) throws IOException;
    public long getDocFreq(String term) throws IOException;
    public String getDocPath(int docID) throws IOException;
    public double getDocNorm(int docID) throws IOException;
}
