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
