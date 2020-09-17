package es.uam.eps.bmi.search.index.freq;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public interface FreqVector extends Iterable<TermFreq> {
    public long size() throws IOException;
    public long getFreq(String term) throws IOException;
}
