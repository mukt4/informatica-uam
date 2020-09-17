package es.uam.eps.bmi.search.index.freq;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public interface TermFreq {
    public String getTerm() throws IOException;
    public long getFreq() throws IOException;
}
