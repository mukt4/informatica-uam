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
package es.uam.eps.bmi.search.index.structure;

import java.io.Serializable;

/**
 *
 * @author pablo
 */
public class Posting implements Comparable<Posting>, Serializable {
    int docID;
    long freq;
    
    public Posting(int id, long f) {
        docID = id;
        freq = f;
    }
    
    public int getDocID() {
        return docID;
    }
    
    public long getFreq() {
        return freq;
    }

    public int compareTo(Posting p) {
        return getDocID() - p.getDocID();
    }
    
    public void add1() {
        freq++;
    }
}
