package es.uam.eps.bmi.search.index;

import es.uam.eps.bmi.search.index.freq.FreqVector;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public interface ForwardIndex extends Index {
    public FreqVector getDocVector(int docID) throws IOException;
    public long getTermFreq(String term, int docID) throws IOException;
}
