package es.uam.eps.bmi.search.index;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public interface DocumentMap {
    public String getDocPath(int docID) throws IOException;
    public double getDocNorm(int docID) throws IOException;
    public int numDocs();
}
