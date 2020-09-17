package es.uam.eps.bmi.search.index;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public interface IndexBuilder {
    public void build(String collectionPath, String indexPath) throws IOException;
    // Métodos que pueden ser útiles en la implementación de un crawler.
    public void init(String indexPath) throws IOException;
    public void indexHTML(String content, String path) throws IOException;
    public void close(String indexPath) throws IOException;
}
