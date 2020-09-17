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
package es.uam.eps.bmi.search.index.structure.lucene;

import es.uam.eps.bmi.search.index.structure.Posting;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.PostingsEnum;
import org.apache.lucene.search.DocIdSetIterator;

/**
 *
 * @author pablo
 */
public class LucenePostingsIterator implements Iterator<Posting> {
    PostingsEnum postings;
    int currentDoc;
    
    public LucenePostingsIterator(PostingsEnum p) throws IOException {
        postings = p;
        currentDoc = postings.nextDoc();
    }
    
    public boolean hasNext() {
        return currentDoc != DocIdSetIterator.NO_MORE_DOCS;
    }
    
    public Posting next() {
        try {
            Posting p = new Posting(postings.docID(),postings.freq());
            currentDoc = postings.nextDoc();
            return p;
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }
}
