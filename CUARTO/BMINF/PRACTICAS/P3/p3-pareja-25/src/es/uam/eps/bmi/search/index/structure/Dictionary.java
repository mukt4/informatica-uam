package es.uam.eps.bmi.search.index.structure;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author pablo
 */
public interface Dictionary extends Serializable {
    public PostingsList getPostings(String term) throws IOException;
    public Collection<String> getAllTerms();
    public long getDocFreq(String term) throws IOException;
}
