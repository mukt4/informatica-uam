package es.uam.eps.bmi.search.index.lucene;

/**
 *
 * @author pablo
 */
public class LuceneForwardIndexBuilder extends LuceneBuilder {
    public LuceneForwardIndexBuilder() {
        type.setStoreTermVectors (true);
    }
}
